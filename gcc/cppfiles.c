/* Part of CPP library.  (include file handling)
   Copyright (C) 1986, 87, 89, 92 - 95, 1998 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987
   Split out of cpplib.c, Zack Weinberg, Oct 1998

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "cpplib.h"

/* The entry points to this file are: find_include_file, finclude,
   append_include_chain, deps_output, and file_cleanup.
   file_cleanup is only called through CPP_BUFFER(pfile)->cleanup,
   so it's static anyway. */

static void add_import			PROTO ((cpp_reader *, int, char *));
static int lookup_import		PROTO ((cpp_reader *, char *,
						struct file_name_list *));
static int redundant_include_p		PROTO ((cpp_reader *, char *));
static struct file_name_map *read_name_map	PROTO ((cpp_reader *, char *));
static char *read_filename_string	PROTO ((int, FILE *));
static int open_include_file		PROTO ((cpp_reader *, char *,
						struct file_name_list *));
static int safe_read			PROTO ((int, char *, int));

/* Not safe to prototype these. */
extern char *xmalloc();
extern char *xrealloc();

/* Append a chain of `struct file_name_list's
   to the end of the main include chain.
   FIRST is the beginning of the chain to append, and LAST is the end.  */

void
append_include_chain (pfile, first, last)
     cpp_reader *pfile;
     struct file_name_list *first, *last;
{
  struct cpp_options *opts = CPP_OPTIONS (pfile);
  struct file_name_list *dir;

  if (!first || !last)
    return;

  if (opts->include == 0)
    opts->include = first;
  else
    opts->last_include->next = first;

  if (opts->first_bracket_include == 0)
    opts->first_bracket_include = first;

  for (dir = first; ; dir = dir->next) {
    int len = strlen (dir->fname) + INCLUDE_LEN_FUDGE;
    if (len > pfile->max_include_len)
      pfile->max_include_len = len;
    if (dir == last)
      break;
  }

  last->next = NULL;
  opts->last_include = last;
}

/* Add output to `deps_buffer' for the -M switch.
   STRING points to the text to be output.
   SPACER is ':' for targets, ' ' for dependencies, zero for text
   to be inserted literally.  */

void
deps_output (pfile, string, spacer)
     cpp_reader *pfile;
     char *string;
     int spacer;
{
  int size;

  if (!*string)
    return;

#ifdef VMS
  hack_vms_include_specification (string);
#endif

  size = strlen (string);

#ifndef MAX_OUTPUT_COLUMNS
#define MAX_OUTPUT_COLUMNS 72
#endif
  if (spacer
      && pfile->deps_column > 0
      && (pfile->deps_column + size) > MAX_OUTPUT_COLUMNS)
    {
      deps_output (pfile, " \\\n  ", 0);
      pfile->deps_column = 0;
    }

  if (pfile->deps_size + size + 8 > pfile->deps_allocated_size)
    {
      pfile->deps_allocated_size = (pfile->deps_size + size + 50) * 2;
      pfile->deps_buffer = (char *) xrealloc (pfile->deps_buffer,
					      pfile->deps_allocated_size);
    }
  if (spacer == ' ' && pfile->deps_column > 0)
    pfile->deps_buffer[pfile->deps_size++] = ' ';
  bcopy (string, &pfile->deps_buffer[pfile->deps_size], size);
  pfile->deps_size += size;
  pfile->deps_column += size;
  if (spacer == ':')
    pfile->deps_buffer[pfile->deps_size++] = ':';
  pfile->deps_buffer[pfile->deps_size] = 0;
}

static int
file_cleanup (pbuf, pfile)
     cpp_buffer *pbuf;
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  if (pbuf->buf)
    {
      free (pbuf->buf);
      pbuf->buf = 0;
    }
  return 0;
}

int
find_include_file (pfile, fbeg, flen, fname,
		   importing, search_start, foundhere)
     cpp_reader *pfile;
     char *fbeg;
     unsigned long flen;
     char *fname;
     int importing;
     struct file_name_list *search_start;
     struct file_name_list **foundhere;
{
  struct file_name_list *searchptr;
  int f;
    
  /* If specified file name is absolute, just open it.  */

  if (*fbeg == '/')
  {
    strcpy (fname, fbeg);
#ifdef VMS
    hack_vms_include_specification (fname);
#endif
    if (redundant_include_p (pfile, fname))
      return -2;
    if (importing)
      f = lookup_import (pfile, fname, NULL_PTR);
    else
      f = open_include_file (pfile, fname, NULL_PTR);
    if (f == -2)
      return -2;	/* Already included this file */
  }
  else
  {
    /* Search directory path, trying to open the file.
       Copy each filename tried into FNAME.  */

    for (searchptr = search_start; searchptr; searchptr = searchptr->next)
    {
      unsigned int l = 0;
      if (searchptr->fname)
      {
	/* The empty string in a search path is ignored.
	   This makes it possible to turn off entirely
	   a standard piece of the list.  */
	if (searchptr->fname[0] == 0)
	  continue;

	l = strlen (searchptr->fname);

	bcopy (searchptr->fname, fname, l);
	fname[l++] = '/';
      }

      bcopy (fbeg, &fname[l], flen);
      fname[flen+l] = '\0';
#ifdef VMS
      hack_vms_include_specification (fname);
#endif /* VMS */
      /* ??? There are currently 3 separate mechanisms for avoiding processing
	 of redundant include files: #import, #pragma once, and
	 redundant_include_p.  It would be nice if they were unified.  */
      if (redundant_include_p (pfile, fname))
	return -2;
      if (importing)
	f = lookup_import (pfile, fname, searchptr);
      else
	f = open_include_file (pfile, fname, searchptr);
      if (f == -2)
	return -2;			/* Already included this file */
#ifdef EACCES
      else if (f == -1 && errno == EACCES)
	cpp_warning (pfile, "Header file %s exists, but is not readable",
		     fname);
#endif
      if (f >= 0)
	break;
    }
  }

  if (f < 0)
    {
      /* A file that was not found.  */
      bcopy (fbeg, fname, flen);
      fname[flen] = 0;

      return -1;
    }
  else
    {
      /* Check to see if this include file is a once-only include file.
	 If so, give up.  */

      struct file_name_list *ptr;

      for (ptr = pfile->dont_repeat_files; ptr; ptr = ptr->next)
	  if (!strcmp (ptr->fname, fname))
	    {
	      close (f);
	      return -2;		/* This file was once'd.  */
	    }
    }

    /* Record file on "seen" list for #import.  */
    add_import (pfile, f, fname);

    *foundhere = searchptr;
    return f;
}

/* Return nonzero if there is no need to include file NAME
   because it has already been included and it contains a conditional
   to make a repeated include do nothing.  */

static int
redundant_include_p (pfile, name)
     cpp_reader *pfile;
     char *name;
{
  struct file_name_list *l = pfile->all_include_files;
  for (; l; l = l->next)
    if (! strcmp (name, l->fname)
	&& l->control_macro
	&& cpp_lookup (pfile, l->control_macro, -1, -1))
      return 1;
  return 0;
}



/* Maintain and search list of included files, for #import.  */

/* Hash a file name for import_hash_table.  */

static int 
import_hash (f)
     char *f;
{
  int val = 0;

  while (*f) val += *f++;
  return (val%IMPORT_HASH_SIZE);
}

/* Search for file FILENAME in import_hash_table.
   Return -2 if found, either a matching name or a matching inode.
   Otherwise, open the file and return a file descriptor if successful
   or -1 if unsuccessful.  */

static int
lookup_import (pfile, filename, searchptr)
     cpp_reader *pfile;
     char *filename;
     struct file_name_list *searchptr;
{
  struct import_file *i;
  int h;
  int hashval;
  struct stat sb;
  int fd;

  hashval = import_hash (filename);

  /* Attempt to find file in list of already included files */
  i = pfile->import_hash_table[hashval];

  while (i) {
    if (!strcmp (filename, i->name))
      return -2;		/* return found */
    i = i->next;
  }
  /* Open it and try a match on inode/dev */
  fd = open_include_file (pfile, filename, searchptr);
  if (fd < 0)
    return fd;
  fstat (fd, &sb);
  for (h = 0; h < IMPORT_HASH_SIZE; h++) {
    i = pfile->import_hash_table[h];
    while (i) {
      /* Compare the inode and the device.
	 Supposedly on some systems the inode is not a scalar.  */
      if (!bcmp ((char *) &i->inode, (char *) &sb.st_ino, sizeof (sb.st_ino))
	  && i->dev == sb.st_dev) {
        close (fd);
        return -2;		/* return found */
      }
      i = i->next;
    }
  }
  return fd;			/* Not found, return open file */
}

/* Add the file FNAME, open on descriptor FD, to import_hash_table.  */

static void
add_import (pfile, fd, fname)
     cpp_reader *pfile;
     int fd;
     char *fname;
{
  struct import_file *i;
  int hashval;
  struct stat sb;

  hashval = import_hash (fname);
  fstat (fd, &sb);
  i = (struct import_file *)xmalloc (sizeof (struct import_file));
  i->name = (char *)xmalloc (strlen (fname)+1);
  strcpy (i->name, fname);
  bcopy ((char *) &sb.st_ino, (char *) &i->inode, sizeof (sb.st_ino));
  i->dev = sb.st_dev;
  i->next = pfile->import_hash_table[hashval];
  pfile->import_hash_table[hashval] = i;
}

/* The file_name_map structure holds a mapping of file names for a
   particular directory.  This mapping is read from the file named
   FILE_NAME_MAP_FILE in that directory.  Such a file can be used to
   map filenames on a file system with severe filename restrictions,
   such as DOS.  The format of the file name map file is just a series
   of lines with two tokens on each line.  The first token is the name
   to map, and the second token is the actual name to use.  */

struct file_name_map
{
  struct file_name_map *map_next;
  char *map_from;
  char *map_to;
};

#define FILE_NAME_MAP_FILE "header.gcc"

/* Read a space delimited string of unlimited length from a stdio
   file.  */

static char *
read_filename_string (ch, f)
     int ch;
     FILE *f;
{
  char *alloc, *set;
  int len;

  len = 20;
  set = alloc = xmalloc (len + 1);
  if (! is_space[ch])
    {
      *set++ = ch;
      while ((ch = getc (f)) != EOF && ! is_space[ch])
	{
	  if (set - alloc == len)
	    {
	      len *= 2;
	      alloc = xrealloc (alloc, len + 1);
	      set = alloc + len / 2;
	    }
	  *set++ = ch;
	}
    }
  *set = '\0';
  ungetc (ch, f);
  return alloc;
}

/* This structure holds a linked list of file name maps, one per directory.  */

struct file_name_map_list
{
  struct file_name_map_list *map_list_next;
  char *map_list_name;
  struct file_name_map *map_list_map;
};

/* Read the file name map file for DIRNAME.  */

static struct file_name_map *
read_name_map (pfile, dirname)
     cpp_reader *pfile;
     char *dirname;
{
  register struct file_name_map_list *map_list_ptr;
  char *name;
  FILE *f;

  for (map_list_ptr = CPP_OPTIONS (pfile)->map_list; map_list_ptr;
       map_list_ptr = map_list_ptr->map_list_next)
    if (! strcmp (map_list_ptr->map_list_name, dirname))
      return map_list_ptr->map_list_map;

  map_list_ptr = ((struct file_name_map_list *)
		  xmalloc (sizeof (struct file_name_map_list)));
  map_list_ptr->map_list_name = savestring (dirname);
  map_list_ptr->map_list_map = NULL;

  name = (char *) alloca (strlen (dirname) + strlen (FILE_NAME_MAP_FILE) + 2);
  strcpy (name, dirname);
  if (*dirname)
    strcat (name, "/");
  strcat (name, FILE_NAME_MAP_FILE);
  f = fopen (name, "r");
  if (!f)
    map_list_ptr->map_list_map = NULL;
  else
    {
      int ch;
      int dirlen = strlen (dirname);

      while ((ch = getc (f)) != EOF)
	{
	  char *from, *to;
	  struct file_name_map *ptr;

	  if (is_space[ch])
	    continue;
	  from = read_filename_string (ch, f);
	  while ((ch = getc (f)) != EOF && is_hor_space[ch])
	    ;
	  to = read_filename_string (ch, f);

	  ptr = ((struct file_name_map *)
		 xmalloc (sizeof (struct file_name_map)));
	  ptr->map_from = from;

	  /* Make the real filename absolute.  */
	  if (*to == '/')
	    ptr->map_to = to;
	  else
	    {
	      ptr->map_to = xmalloc (dirlen + strlen (to) + 2);
	      strcpy (ptr->map_to, dirname);
	      ptr->map_to[dirlen] = '/';
	      strcpy (ptr->map_to + dirlen + 1, to);
	      free (to);
	    }	      

	  ptr->map_next = map_list_ptr->map_list_map;
	  map_list_ptr->map_list_map = ptr;

	  while ((ch = getc (f)) != '\n')
	    if (ch == EOF)
	      break;
	}
      fclose (f);
    }
  
  map_list_ptr->map_list_next = CPP_OPTIONS (pfile)->map_list;
  CPP_OPTIONS (pfile)->map_list = map_list_ptr;

  return map_list_ptr->map_list_map;
}  

/* Try to open include file FILENAME.  SEARCHPTR is the directory
   being tried from the include file search path.  This function maps
   filenames on file systems based on information read by
   read_name_map.  */

static int
open_include_file (pfile, filename, searchptr)
     cpp_reader *pfile;
     char *filename;
     struct file_name_list *searchptr;
{
  if (CPP_OPTIONS (pfile)->remap)
    {
      register struct file_name_map *map;
      register char *from;
      char *p, *dir;

      if (searchptr && ! searchptr->got_name_map)
	{
	  searchptr->name_map = read_name_map (pfile,
					       searchptr->fname
					       ? searchptr->fname : ".");
	  searchptr->got_name_map = 1;
	}

      /* First check the mapping for the directory we are using.  */
      if (searchptr && searchptr->name_map)
	{
	  from = filename;
	  if (searchptr->fname)
	    from += strlen (searchptr->fname) + 1;
	  for (map = searchptr->name_map; map; map = map->map_next)
	    {
	      if (! strcmp (map->map_from, from))
		{
		  /* Found a match.  */
		  return open (map->map_to, O_RDONLY, 0666);
		}
	    }
	}

      /* Try to find a mapping file for the particular directory we are
	 looking in.  Thus #include <sys/types.h> will look up sys/types.h
	 in /usr/include/header.gcc and look up types.h in
	 /usr/include/sys/header.gcc.  */
      p = rindex (filename, '/');
      if (! p)
	p = filename;
      if (searchptr
	  && searchptr->fname
	  && strlen (searchptr->fname) == (size_t) (p - filename)
	  && ! strncmp (searchptr->fname, filename, p - filename))
	{
	  /* FILENAME is in SEARCHPTR, which we've already checked.  */
	  return open (filename, O_RDONLY, 0666);
	}

      if (p == filename)
	{
	  dir = ".";
	  from = filename;
	}
      else
	{
	  dir = (char *) alloca (p - filename + 1);
	  bcopy (filename, dir, p - filename);
	  dir[p - filename] = '\0';
	  from = p + 1;
	}
      for (map = read_name_map (pfile, dir); map; map = map->map_next)
	if (! strcmp (map->map_from, from))
	  return open (map->map_to, O_RDONLY, 0666);
    }

  return open (filename, O_RDONLY, 0666);
}

/* Process the contents of include file FNAME, already open on descriptor F,
   with output to OP.
   SYSTEM_HEADER_P is 1 if this file resides in any one of the known
   "system" include directories (as decided by the `is_system_include'
   function above).
   DIRPTR is the link in the dir path through which this file was found,
   or 0 if the file name was absolute or via the current directory.
   Return 1 on success, 0 on failure.

   The caller is responsible for the cpp_push_buffer.  */

int
finclude (pfile, f, fname, system_header_p, dirptr)
     cpp_reader *pfile;
     int f;
     char *fname;
     int system_header_p;
     struct file_name_list *dirptr;
{
  struct stat st;
  size_t st_size;
  long i;
  int length;
  cpp_buffer *fp;			/* For input stack frame */
#if 0
  int missing_newline = 0;
#endif

  if (fstat (f, &st) < 0)
    {
      cpp_perror_with_name (pfile, fname);
      close (f);
      cpp_pop_buffer (pfile);
      return 0;
    }

  fp = CPP_BUFFER (pfile);
  fp->nominal_fname = fp->fname = fname;
#if 0
  fp->length = 0;
#endif
  fp->dir = dirptr;
  fp->system_header_p = system_header_p;
  fp->lineno = 1;
  fp->colno = 1;
  fp->cleanup = file_cleanup;

  if (S_ISREG (st.st_mode)) {
    st_size = (size_t) st.st_size;
    if (st_size != st.st_size || st_size + 2 < st_size) {
      cpp_error (pfile, "file `%s' too large", fname);
      close (f);
      return 0;
    }
    fp->buf = (U_CHAR *) xmalloc (st_size + 2);
    fp->alimit = fp->buf + st_size + 2;
    fp->cur = fp->buf;

    /* Read the file contents, knowing that st_size is an upper bound
       on the number of bytes we can read.  */
    length = safe_read (f, fp->buf, st_size);
    fp->rlimit = fp->buf + length;
    if (length < 0) goto nope;
  }
  else if (S_ISDIR (st.st_mode)) {
    cpp_error (pfile, "directory `%s' specified in #include", fname);
    close (f);
    return 0;
  } else {
    /* Cannot count its file size before reading.
       First read the entire file into heap and
       copy them into buffer on stack.  */

    size_t bsize = 2000;

    st_size = 0;
    fp->buf = (U_CHAR *) xmalloc (bsize + 2);

    for (;;) {
      i = safe_read (f, fp->buf + st_size, bsize - st_size);
      if (i < 0)
	goto nope;      /* error! */
      st_size += i;
      if (st_size != bsize)
	break;	/* End of file */
      bsize *= 2;
      fp->buf = (U_CHAR *) xrealloc (fp->buf, bsize + 2);
    }
    fp->cur = fp->buf;
    length = st_size;
  }

  if ((length > 0 && fp->buf[length - 1] != '\n')
      /* Backslash-newline at end is not good enough.  */
      || (length > 1 && fp->buf[length - 2] == '\\')) {
    fp->buf[length++] = '\n';
#if 0
    missing_newline = 1;
#endif
  }
  fp->buf[length] = '\0';
  fp->rlimit = fp->buf + length;

  /* Close descriptor now, so nesting does not use lots of descriptors.  */
  close (f);

  /* Must do this before calling trigraph_pcp, so that the correct file name
     will be printed in warning messages.  */

  pfile->input_stack_listing_current = 0;

#if 0
  if (!no_trigraphs)
    trigraph_pcp (fp);
#endif

#if 0
  rescan (op, 0);

  if (missing_newline)
    fp->lineno--;

  if (CPP_PEDANTIC (pfile) && missing_newline)
    pedwarn ("file does not end in newline");

  indepth--;
  input_file_stack_tick++;
  free (fp->buf);
#endif
  return 1;

 nope:

  cpp_perror_with_name (pfile, fname);
  close (f);
  free (fp->buf);
  return 1;
}

/* Read LEN bytes at PTR from descriptor DESC, for file FILENAME,
   retrying if necessary.  If MAX_READ_LEN is defined, read at most
   that bytes at a time.  Return a negative value if an error occurs,
   otherwise return the actual number of bytes read,
   which must be LEN unless end-of-file was reached.  */

static int
safe_read (desc, ptr, len)
     int desc;
     char *ptr;
     int len;
{
  int left, rcount, nchars;

  left = len;
  while (left > 0) {
    rcount = left;
#ifdef MAX_READ_LEN
    if (rcount > MAX_READ_LEN)
      rcount = MAX_READ_LEN;
#endif
    nchars = read (desc, ptr, rcount);
    if (nchars < 0)
      {
#ifdef EINTR
	if (errno == EINTR)
	  continue;
#endif
	return nchars;
      }
    if (nchars == 0)
      break;
    ptr += nchars;
    left -= nchars;
  }
  return len - left;
}

#ifdef VMS

/* Under VMS we need to fix up the "include" specification filename.

   Rules for possible conversions

	fullname		tried paths

	name			name
	./dir/name		[.dir]name
	/dir/name		dir:name
	/name			[000000]name, name
	dir/name		dir:[000000]name, dir:name, dir/name
	dir1/dir2/name		dir1:[dir2]name, dir1:[000000.dir2]name
	path:/name		path:[000000]name, path:name
	path:/dir/name		path:[000000.dir]name, path:[dir]name
	path:dir/name		path:[dir]name
	[path]:[dir]name	[path.dir]name
	path/[dir]name		[path.dir]name

   The path:/name input is constructed when expanding <> includes. */


static void
hack_vms_include_specification (fullname)
     char *fullname;
{
  register char *basename, *unixname, *local_ptr, *first_slash;
  int f, check_filename_before_returning, must_revert;
  char Local[512];

  check_filename_before_returning = 0;
  must_revert = 0;
  /* See if we can find a 1st slash. If not, there's no path information.  */
  first_slash = index (fullname, '/');
  if (first_slash == 0)
    return 0;				/* Nothing to do!!! */

  /* construct device spec if none given.  */

  if (index (fullname, ':') == 0)
    {

      /* If fullname has a slash, take it as device spec.  */

      if (first_slash == fullname)
	{
	  first_slash = index (fullname+1, '/');	/* 2nd slash ? */
	  if (first_slash)
	    *first_slash = ':';				/* make device spec  */
	  for (basename = fullname; *basename != 0; basename++)
	    *basename = *(basename+1);			/* remove leading slash  */
	}
      else if ((first_slash[-1] != '.')		/* keep ':/', './' */
	    && (first_slash[-1] != ':')
	    && (first_slash[-1] != ']'))	/* or a vms path  */
	{
	  *first_slash = ':';
	}
      else if ((first_slash[1] == '[')		/* skip './' in './[dir'  */
	    && (first_slash[-1] == '.'))
	fullname += 2;
    }

  /* Get part after first ':' (basename[-1] == ':')
     or last '/' (basename[-1] == '/').  */

  basename = base_name (fullname);

  local_ptr = Local;			/* initialize */

  /* We are trying to do a number of things here.  First of all, we are
     trying to hammer the filenames into a standard format, such that later
     processing can handle them.
     
     If the file name contains something like [dir.], then it recognizes this
     as a root, and strips the ".]".  Later processing will add whatever is
     needed to get things working properly.
     
     If no device is specified, then the first directory name is taken to be
     a device name (or a rooted logical).  */

  /* Point to the UNIX filename part (which needs to be fixed!)
     but skip vms path information.
     [basename != fullname since first_slash != 0].  */

  if ((basename[-1] == ':')		/* vms path spec.  */
      || (basename[-1] == ']')
      || (basename[-1] == '>'))
    unixname = basename;
  else
    unixname = fullname;

  if (*unixname == '/')
    unixname++;

  /* If the directory spec is not rooted, we can just copy
     the UNIX filename part and we are done.  */

  if (((basename - fullname) > 1)
     && (  (basename[-1] == ']')
        || (basename[-1] == '>')))
    {
      if (basename[-2] != '.')
	{

	/* The VMS part ends in a `]', and the preceding character is not a `.'.
	   -> PATH]:/name (basename = '/name', unixname = 'name')
	   We strip the `]', and then splice the two parts of the name in the
	   usual way.  Given the default locations for include files in cccp.c,
	   we will only use this code if the user specifies alternate locations
	   with the /include (-I) switch on the command line.  */

	  basename -= 1;	/* Strip "]" */
	  unixname--;		/* backspace */
	}
      else
	{

	/* The VMS part has a ".]" at the end, and this will not do.  Later
	   processing will add a second directory spec, and this would be a syntax
	   error.  Thus we strip the ".]", and thus merge the directory specs.
	   We also backspace unixname, so that it points to a '/'.  This inhibits the
	   generation of the 000000 root directory spec (which does not belong here
	   in this case).  */

	  basename -= 2;	/* Strip ".]" */
	  unixname--;		/* backspace */
	}
    }

  else

    {

      /* We drop in here if there is no VMS style directory specification yet.
         If there is no device specification either, we make the first dir a
         device and try that.  If we do not do this, then we will be essentially
         searching the users default directory (as if they did a #include "asdf.h").
        
         Then all we need to do is to push a '[' into the output string. Later
         processing will fill this in, and close the bracket.  */

      if ((unixname != fullname)	/* vms path spec found.  */
	 && (basename[-1] != ':'))
	*local_ptr++ = ':';		/* dev not in spec.  take first dir */

      *local_ptr++ = '[';		/* Open the directory specification */
    }

    if (unixname == fullname)		/* no vms dir spec.  */
      {
	must_revert = 1;
	if ((first_slash != 0)		/* unix dir spec.  */
	    && (*unixname != '/')	/* not beginning with '/'  */
	    && (*unixname != '.'))	/* or './' or '../'  */
	  *local_ptr++ = '.';		/* dir is local !  */
      }

  /* at this point we assume that we have the device spec, and (at least
     the opening "[" for a directory specification.  We may have directories
     specified already.

     If there are no other slashes then the filename will be
     in the "root" directory.  Otherwise, we need to add
     directory specifications.  */

  if (index (unixname, '/') == 0)
    {
      /* if no directories specified yet and none are following.  */
      if (local_ptr[-1] == '[')
	{
	  /* Just add "000000]" as the directory string */
	  strcpy (local_ptr, "000000]");
	  local_ptr += strlen (local_ptr);
	  check_filename_before_returning = 1; /* we might need to fool with this later */
	}
    }
  else
    {

      /* As long as there are still subdirectories to add, do them.  */
      while (index (unixname, '/') != 0)
	{
	  /* If this token is "." we can ignore it
	       if it's not at the beginning of a path.  */
	  if ((unixname[0] == '.') && (unixname[1] == '/'))
	    {
	      /* remove it at beginning of path.  */
	      if (  ((unixname == fullname)		/* no device spec  */
		    && (fullname+2 != basename))	/* starts with ./ */
							/* or  */
		 || ((basename[-1] == ':')		/* device spec  */
		    && (unixname-1 == basename)))	/* and ./ afterwards  */
		*local_ptr++ = '.';		 	/* make '[.' start of path.  */
	      unixname += 2;
	      continue;
	    }

	  /* Add a subdirectory spec. Do not duplicate "." */
	  if (  local_ptr[-1] != '.'
	     && local_ptr[-1] != '['
	     && local_ptr[-1] != '<')
	    *local_ptr++ = '.';

	  /* If this is ".." then the spec becomes "-" */
	  if (  (unixname[0] == '.')
	     && (unixname[1] == '.')
	     && (unixname[2] == '/'))
	    {
	      /* Add "-" and skip the ".." */
	      if ((local_ptr[-1] == '.')
		  && (local_ptr[-2] == '['))
		local_ptr--;			/* prevent [.-  */
	      *local_ptr++ = '-';
	      unixname += 3;
	      continue;
	    }

	  /* Copy the subdirectory */
	  while (*unixname != '/')
	    *local_ptr++= *unixname++;

	  unixname++;			/* Skip the "/" */
	}

      /* Close the directory specification */
      if (local_ptr[-1] == '.')		/* no trailing periods */
	local_ptr--;

      if (local_ptr[-1] == '[')		/* no dir needed */
	local_ptr--;
      else
	*local_ptr++ = ']';
    }

  /* Now add the filename.  */

  while (*unixname)
    *local_ptr++ = *unixname++;
  *local_ptr = 0;

  /* Now append it to the original VMS spec.  */

  strcpy ((must_revert==1)?fullname:basename, Local);

  /* If we put a [000000] in the filename, try to open it first. If this fails,
     remove the [000000], and return that name.  This provides flexibility
     to the user in that they can use both rooted and non-rooted logical names
     to point to the location of the file.  */

  if (check_filename_before_returning)
    {
      f = open (fullname, O_RDONLY, 0666);
      if (f >= 0)
	{
	  /* The file name is OK as it is, so return it as is.  */
	  close (f);
	  return 1;
	}

      /* The filename did not work.  Try to remove the [000000] from the name,
	 and return it.  */

      basename = index (fullname, '[');
      local_ptr = index (fullname, ']') + 1;
      strcpy (basename, local_ptr);		/* this gets rid of it */

    }

  return 1;
}
#endif	/* VMS */
