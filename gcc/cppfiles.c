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
   include_hash, append_include_chain, deps_output, and file_cleanup.
   file_cleanup is only called through CPP_BUFFER(pfile)->cleanup,
   so it's static anyway. */

static struct include_hash *redundant_include_p
					PROTO ((cpp_reader *,
						struct include_hash *,
						struct file_name_list *));
static struct file_name_map *read_name_map	PROTO ((cpp_reader *, char *));
static char *read_filename_string	PROTO ((int, FILE *));
static char *remap_filename 		PROTO ((cpp_reader *, char *,
						struct file_name_list *));
static long safe_read			PROTO ((int, char *, int));
static void simplify_pathname		PROTO ((char *));

#if 0
static void hack_vms_include_specification PROTO ((char *));
#endif

/* Windows does not natively support inodes, and neither does MSDOS.
   VMS has non-numeric inodes. */
#ifdef VMS
#define INO_T_EQ(a, b) (!bcmp((char *) &(a), (char *) &(b), sizeof (a)))
#elif (defined _WIN32 && !defined CYGWIN) || defined __MSDOS__
#define INO_T_EQ(a, b) 0
#else
#define INO_T_EQ(a, b) ((a) == (b))
#endif

/* Append an entry for dir DIR to list LIST, simplifying it if
   possible.  SYS says whether this is a system include directory.
   *** DIR is modified in place.  It must be writable and permanently
   allocated. LIST is a pointer to the head pointer, because we actually
   *prepend* the dir, and reverse the list later (in merge_include_chains). */
void
append_include_chain (pfile, list, dir, sysp)
     cpp_reader *pfile;
     struct file_name_list **list;
     char *dir;
     int sysp;
{
  struct file_name_list *new;
  struct stat st;
  unsigned int len;

  dir = savestring (dir);
  simplify_pathname (dir);
  if (stat (dir, &st))
    {
      /* Dirs that don't exist are silently ignored. */
      if (errno != ENOENT)
	cpp_perror_with_name (pfile, dir);
      return;
    }

  if (!S_ISDIR (st.st_mode))
    {
      cpp_message (pfile, 1, "%s: %s: Not a directory", progname, dir);
      return;
    }

  len = strlen(dir);
  if (len > pfile->max_include_len)
    pfile->max_include_len = len;
  
  new = (struct file_name_list *)xmalloc (sizeof (struct file_name_list));
  new->name = dir;
  new->nlen = len;
  new->next = *list;
  new->ino  = st.st_ino;
  new->dev  = st.st_dev;
  new->sysp = sysp;
  new->name_map = NULL;

  *list = new;
}

/* Merge the four include chains together in the order quote, bracket,
   system, after.  Remove duplicate dirs (as determined by
   INO_T_EQ()).  The system_include and after_include chains are never
   referred to again after this function; all access is through the
   bracket_include path.

   For the future: Check if the directory is empty (but
   how?) and possibly preload the include hash. */

void
merge_include_chains (opts)
     struct cpp_options *opts;
{
  struct file_name_list *prev, *next, *cur, *other;
  struct file_name_list *quote, *brack, *systm, *after;
  struct file_name_list *qtail, *btail, *stail, *atail;

  qtail = opts->quote_include;
  btail = opts->bracket_include;
  stail = opts->system_include;
  atail = opts->after_include;

  /* Nreverse the four lists. */
  prev = 0;
  for (cur = qtail; cur; cur = next)
    {
      next = cur->next;
      cur->next = prev;
      prev = cur;
    }
  quote = prev;

  prev = 0;
  for (cur = btail; cur; cur = next)
    {
      next = cur->next;
      cur->next = prev;
      prev = cur;
    }
  brack = prev;

  prev = 0;
  for (cur = stail; cur; cur = next)
    {
      next = cur->next;
      cur->next = prev;
      prev = cur;
    }
  systm = prev;

  prev = 0;
  for (cur = atail; cur; cur = next)
    {
      next = cur->next;
      cur->next = prev;
      prev = cur;
    }
  after = prev;

  /* Paste together bracket, system, and after include chains. */
  if (stail)
    stail->next = after;
  else
    systm = after;
  if (btail)
    btail->next = systm;
  else
    brack = systm;

  /* This is a bit tricky.
     First we drop dupes from the quote-include list.
     Then we drop dupes from the bracket-include list.
     Finally, if qtail and brack are the same directory,
     we cut out qtail.

     We can't just merge the lists and then uniquify them because
     then we may lose directories from the <> search path that should
     be there; consider -Ifoo -Ibar -I- -Ifoo -Iquux. It is however
     safe to treat -Ibar -Ifoo -I- -Ifoo -Iquux as if written
     -Ibar -I- -Ifoo -Iquux. */

  for (cur = quote; cur; cur = cur->next)
    {
      for (other = quote; other != cur; other = other->next)
        if (INO_T_EQ (cur->ino, other->ino)
	    && cur->dev == other->dev)
          {
	    prev->next = cur->next;
	    free (cur->name);
	    free (cur);
	    cur = prev;
	    break;
	  }
      prev = cur;
    }
  qtail = prev;

  for (cur = brack; cur; cur = cur->next)
    {
      for (other = brack; other != cur; other = other->next)
        if (INO_T_EQ (cur->ino, other->ino)
	    && cur->dev == other->dev)
          {
	    prev->next = cur->next;
	    free (cur->name);
	    free (cur);
	    cur = prev;
	    break;
	  }
      prev = cur;
    }

  if (quote)
    {
      if (INO_T_EQ (qtail->ino, brack->ino) && qtail->dev == brack->dev)
        {
	  if (quote == qtail)
	    {
	      free (quote->name);
	      free (quote);
	      quote = brack;
	    }
	  else
	    {
	      cur = quote;
	      while (cur->next != qtail)
		  cur = cur->next;
	      cur->next = brack;
	      free (qtail->name);
	      free (qtail);
	    }
	}
      else
	  qtail->next = brack;
    }
  else
      quote = brack;

  opts->quote_include = quote;
  opts->bracket_include = brack;
  opts->system_include = NULL;
  opts->after_include = NULL;
}

/* Look up or add an entry to the table of all includes.  This table
 is indexed by the name as it appears in the #include line.  The
 ->next_this_file chain stores all different files with the same
 #include name (there are at least three ways this can happen).  The
 hash function could probably be improved a bit. */

struct include_hash *
include_hash (pfile, fname, add)
     cpp_reader *pfile;
     char *fname;
     int add;
{
  unsigned int hash = 0;
  struct include_hash *l, *m;
  char *f = fname;

  while (*f)
    hash += *f++;

  l = pfile->all_include_files[hash % ALL_INCLUDE_HASHSIZE];
  m = 0;
  for (; l; m = l, l = l->next)
    if (!strcmp (l->nshort, fname))
      return l;

  if (!add)
    return 0;
  
  l = (struct include_hash *) xmalloc (sizeof (struct include_hash));
  l->next = NULL;
  l->next_this_file = NULL;
  l->foundhere = NULL;
  l->buf = NULL;
  l->limit = NULL;
  if (m)
    m->next = l;
  else
    pfile->all_include_files[hash % ALL_INCLUDE_HASHSIZE] = l;
  
  return l;
}

/* Return 0 if the file pointed to by IHASH has never been included before,
         -1 if it has been included before and need not be again,
	 or a pointer to an IHASH entry which is the file to be reread.
   "Never before" is with respect to the position in ILIST.

   This will not detect redundancies involving odd uses of the
   `current directory' rule for "" includes.  They aren't quite
   pathological, but I think they are rare enough not to worry about.
   The simplest example is:

   top.c:
   #include "a/a.h"
   #include "b/b.h"

   a/a.h:
   #include "../b/b.h"

   and the problem is that for `current directory' includes,
   ihash->foundhere is not on any of the global include chains,
   so the test below (i->foundhere == l) may be false even when
   the directories are in fact the same.  */

static struct include_hash *
redundant_include_p (pfile, ihash, ilist)
     cpp_reader *pfile;
     struct include_hash *ihash;
     struct file_name_list *ilist;
{
  struct file_name_list *l;
  struct include_hash *i;

  if (! ihash->foundhere)
    return 0;

  for (i = ihash; i; i = i->next_this_file)
    for (l = ilist; l; l = l->next)
       if (i->foundhere == l)
	 /* The control_macro works like this: If it's NULL, the file
	    is to be included again.  If it's "", the file is never to
	    be included again.  If it's a string, the file is not to be
	    included again if the string is the name of a defined macro. */
	 return (i->control_macro
		 && (i->control_macro[0] == '\0'
		     || cpp_lookup (pfile, i->control_macro, -1, -1)))
	     ? (struct include_hash *)-1 : i;

  return 0;
}

static int
file_cleanup (pbuf, pfile)
     cpp_buffer *pbuf;
     cpp_reader *pfile;
{
  if (pbuf->buf)
    {
      free (pbuf->buf);
      pbuf->buf = 0;
    }
  if (pfile->system_include_depth)
    pfile->system_include_depth--;
  return 0;
}

/* Search for include file FNAME in the include chain starting at
   SEARCH_START.  Return -2 if this file doesn't need to be included
   (because it was included already and it's marked idempotent),
   -1 if an error occurred, or a file descriptor open on the file.
   *IHASH is set to point to the include hash entry for this file, and
   *BEFORE is 1 if the file was included before (but needs to be read
   again). */
int
find_include_file (pfile, fname, search_start, ihash, before)
     cpp_reader *pfile;
     char *fname;
     struct file_name_list *search_start;
     struct include_hash **ihash;
     int *before;
{
  struct file_name_list *l;
  struct include_hash *ih, *jh;
  int f, len;
  char *name;
  
  ih = include_hash (pfile, fname, 1);
  jh = redundant_include_p (pfile, ih,
			    fname[0] == '/' ? ABSOLUTE_PATH : search_start);

  if (jh != 0)
    {
      *before = 1;
      *ihash = jh;

      if (jh == (struct include_hash *)-1)
	return -2;
      else
	return open (jh->name, O_RDONLY, 0666);
    }

  if (ih->foundhere)
    /* A file is already known by this name, but it's not the same file.
       Allocate another include_hash block and add it to the next_this_file
       chain. */
    {
      jh = (struct include_hash *)xmalloc (sizeof (struct include_hash));
      while (ih->next_this_file) ih = ih->next_this_file;

      ih->next_this_file = jh;
      jh = ih;
      ih = ih->next_this_file;

      ih->next = NULL;
      ih->next_this_file = NULL;
      ih->buf = NULL;
      ih->limit = NULL;
    }
  *before = 0;
  *ihash = ih;
  ih->nshort = savestring (fname);
  ih->control_macro = NULL;
  
  /* If the pathname is absolute, just open it. */ 
  if (fname[0] == '/')
    {
      ih->foundhere = ABSOLUTE_PATH;
      ih->name = ih->nshort;
      return open (ih->name, O_RDONLY, 0666);
    }

  /* Search directory path, trying to open the file. */

  /* The first entry in the search list may be a buffer-specific entry,
     and its directory name may be longer than max_include_len.  Adjust
     as appropriate. */
 if (pfile->max_include_len < search_start->nlen)
    pfile->max_include_len = search_start->nlen;
  len = strlen (fname);
  name = xmalloc (len + pfile->max_include_len + 2 + INCLUDE_LEN_FUDGE);

  for (l = search_start; l; l = l->next)
    {
      bcopy (l->name, name, l->nlen);
      name[l->nlen] = '/';
      strcpy (&name[l->nlen+1], fname);
      simplify_pathname (name);
      if (CPP_OPTIONS (pfile)->remap)
	name = remap_filename (pfile, name, l);
      
      f = open (name, O_RDONLY, 0666);
#ifdef EACCES
      if (f == -1 && errno == EACCES)
	{
	  cpp_error(pfile, "included file `%s' exists but is not readable",
		    name);
	  return -1;
	}
#endif

      if (f >= 0)
        {
	  ih->foundhere = l;
	  ih->name = xrealloc (name, strlen (name)+1);
	  return f;
        }
    }
  
    if (jh)
      {
	jh->next_this_file = NULL;
	free (ih);
      }
    free (name);
    *ihash = (struct include_hash *)-1;
    return -1;
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

  name = (char *) alloca (strlen (dirname) + strlen (FILE_NAME_MAP_FILE) + 2);
  strcpy (name, dirname);
  if (*dirname)
    strcat (name, "/");
  strcat (name, FILE_NAME_MAP_FILE);
  f = fopen (name, "r");
  if (!f)
    map_list_ptr->map_list_map = (struct file_name_map *)-1;
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

/* Remap NAME based on the file_name_map (if any) for LOC. */

static char *
remap_filename (pfile, name, loc)
     cpp_reader *pfile;
     char *name;
     struct file_name_list *loc;
{
  struct file_name_map *map;
  char *from;
  char *p, *dir;

  if (! loc->name_map)
    loc->name_map = read_name_map (pfile,
				   loc->name
				   ? loc->name : ".");

  if (loc->name_map == (struct file_name_map *)-1)
    return name;
  
  from = name + strlen (loc->name) + 1;
  
  for (map = loc->name_map; map; map = map->map_next)
    if (!strcmp (map->map_from, from))
      return map->map_to;

  /* Try to find a mapping file for the particular directory we are
     looking in.  Thus #include <sys/types.h> will look up sys/types.h
     in /usr/include/header.gcc and look up types.h in
     /usr/include/sys/header.gcc.  */
  p = rindex (name, '/');
  if (!p)
    p = name;
  if (loc && loc->name
      && strlen (loc->name) == (size_t) (p - name)
      && !strncmp (loc->name, name, p - name))
    /* FILENAME is in SEARCHPTR, which we've already checked.  */
    return name;

  if (p == name)
    {
      dir = ".";
      from = name;
    }
  else
    {
      dir = (char *) alloca (p - name + 1);
      bcopy (name, dir, p - name);
      dir[p - name] = '\0';
      from = p + 1;
    }
  
  for (map = read_name_map (pfile, dir); map; map = map->map_next)
    if (! strcmp (map->map_from, name))
      return map->map_to;

  return name;
}

/* Read the contents of FD into the buffer on the top of PFILE's stack.
   IHASH points to the include hash entry for the file associated with
   FD.

   The caller is responsible for the cpp_push_buffer.  */

int
finclude (pfile, fd, ihash)
     cpp_reader *pfile;
     int fd;
     struct include_hash *ihash;
{
  struct stat st;
  size_t st_size;
  long i, length;
  cpp_buffer *fp;

  if (fstat (fd, &st) < 0)
    goto perror_fail;
  
  fp = CPP_BUFFER (pfile);
  fp->nominal_fname = fp->fname = ihash->name;
  fp->ihash = ihash;
  fp->system_header_p = (ihash->foundhere != ABSOLUTE_PATH
			 && ihash->foundhere->sysp);
  fp->lineno = 1;
  fp->colno = 1;
  fp->cleanup = file_cleanup;

  /* The ->dir field is only used when ignore_srcdir is not in effect;
     see do_include */
  if (!CPP_OPTIONS (pfile)->ignore_srcdir)
    {
      char *last_slash;
      fp->dir = savestring (fp->fname);
      last_slash = rindex (fp->dir, '/');
      if (last_slash)
        {
	  if (last_slash == fp->dir)
	    {
	      fp->dlen = 1;
	      last_slash[1] = '\0';
	    }
	  else
	    {
	      fp->dlen = last_slash - fp->dir;
	      *last_slash = '\0';
	    }
	}
      else
        {
	  fp->dir[0] = '.';
	  fp->dir[1] = '\0';
	  fp->dlen = 1;
	}
    }

  if (S_ISREG (st.st_mode))
    {
      st_size = (size_t) st.st_size;
      if (st_size != st.st_size || st_size + 2 < st_size)
      {
        cpp_error (pfile, "file `%s' too large", ihash->name);
	goto fail;
      }
      fp->buf = (U_CHAR *) xmalloc (st_size + 2);
      fp->alimit = fp->buf + st_size + 2;
      fp->cur = fp->buf;
      
      /* Read the file contents, knowing that st_size is an upper bound
	 on the number of bytes we can read.  */
      length = safe_read (fd, fp->buf, st_size);
      fp->rlimit = fp->buf + length;
      if (length < 0)
	  goto perror_fail;
    }
  else if (S_ISDIR (st.st_mode))
    {
      cpp_pop_buffer (pfile);
      cpp_error (pfile, "directory `%s' specified in #include", ihash->name);
      goto fail;
    }
  else
    {
      /* Cannot count its file size before reading.
	 First read the entire file into heap and
	 copy them into buffer on stack.  */

      size_t bsize = 2000;

      st_size = 0;
      fp->buf = (U_CHAR *) xmalloc (bsize + 2);

      for (;;)
        {
	  i = safe_read (fd, fp->buf + st_size, bsize - st_size);
	  if (i < 0)
	    goto perror_fail;
	  st_size += i;
	  if (st_size != bsize)
	    break;	/* End of file */
	  bsize *= 2;
	  fp->buf = (U_CHAR *) xrealloc (fp->buf, bsize + 2);
	}
      fp->cur = fp->buf;
      length = st_size;
    }

  /* FIXME: Broken in presence of trigraphs (consider ??/<EOF>)
     and doesn't warn about a missing newline. */
  if ((length > 0 && fp->buf[length - 1] != '\n')
      || (length > 1 && fp->buf[length - 2] == '\\'))
    fp->buf[length++] = '\n';

  fp->buf[length] = '\0';
  fp->rlimit = fp->buf + length;

  close (fd);
  pfile->input_stack_listing_current = 0;
  return 1;

 perror_fail:
  cpp_pop_buffer (pfile);
  cpp_error_from_errno (pfile, ihash->name);
 fail:
  close (fd);
  return 0;
}

/* Read LEN bytes at PTR from descriptor DESC, for file FILENAME,
   retrying if necessary.  If MAX_READ_LEN is defined, read at most
   that bytes at a time.  Return a negative value if an error occurs,
   otherwise return the actual number of bytes read,
   which must be LEN unless end-of-file was reached.  */

static long
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
  int cr = 0;

  if (!*string)
    return;

  size = strlen (string);

#ifndef MAX_OUTPUT_COLUMNS
#define MAX_OUTPUT_COLUMNS 72
#endif
  if (pfile->deps_column > 0
      && (pfile->deps_column + size) > MAX_OUTPUT_COLUMNS)
    {
      size += 5;
      cr = 1;
      pfile->deps_column = 0;
    }

  if (pfile->deps_size + size + 8 > pfile->deps_allocated_size)
    {
      pfile->deps_allocated_size = (pfile->deps_size + size + 50) * 2;
      pfile->deps_buffer = (char *) xrealloc (pfile->deps_buffer,
					      pfile->deps_allocated_size);
    }

  if (cr)
    {
      bcopy (" \\\n  ", &pfile->deps_buffer[pfile->deps_size], 5);
      pfile->deps_size += 5;
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

/* Simplify a path name in place, deleting redundant components.  This
   reduces OS overhead and guarantees that equivalent paths compare
   the same (modulo symlinks).

   Transforms made:
   foo/bar/../quux	foo/quux
   foo/./bar		foo/bar
   foo//bar		foo/bar
   /../quux		/quux
   //quux		//quux  (POSIX allows leading // as a namespace escape)

   Guarantees no trailing slashes. All transforms reduce the length
   of the string.
 */
static void
simplify_pathname (path)
    char *path;
{
    char *from, *to;
    char *base;
    int absolute = 0;

#if defined _WIN32 || defined __MSDOS__
    /* Convert all backslashes to slashes. */
    for (from = path; *from; from++)
	if (*from == '\\') *from = '/';
    
    /* Skip over leading drive letter if present. */
    if (ISALPHA (path[0]) && path[1] == ':')
	from = to = &path[2];
    else
	from = to = path;
#else
    from = to = path;
#endif
    
    /* Remove redundant initial /s.  */
    if (*from == '/')
    {
	absolute = 1;
	to++;
	from++;
	if (*from == '/')
	{
	    if (*++from == '/')
		/* 3 or more initial /s are equivalent to 1 /.  */
		while (*++from == '/');
	    else
		/* On some hosts // differs from /; Posix allows this.  */
		to++;
	}
    }
    base = to;
    
    for (;;)
    {
	while (*from == '/')
	    from++;

	if (from[0] == '.' && from[1] == '/')
	    from += 2;
	else if (from[0] == '.' && from[1] == '\0')
	    goto done;
	else if (from[0] == '.' && from[1] == '.' && from[2] == '/')
	{
	    if (base == to)
	    {
		if (absolute)
		    from += 3;
		else
		{
		    *to++ = *from++;
		    *to++ = *from++;
		    *to++ = *from++;
		    base = to;
		}
	    }
	    else
	    {
		to -= 2;
		while (to > base && *to != '/') to--;
		if (*to == '/')
		    to++;
		from += 3;
	    }
	}
	else if (from[0] == '.' && from[1] == '.' && from[2] == '\0')
	{
	    if (base == to)
	    {
		if (!absolute)
		{
		    *to++ = *from++;
		    *to++ = *from++;
		}
	    }
	    else
	    {
		to -= 2;
		while (to > base && *to != '/') to--;
		if (*to == '/')
		    to++;
	    }
	    goto done;
	}
	else
	    /* Copy this component and trailing /, if any.  */
	    while ((*to++ = *from++) != '/')
	    {
		if (!to[-1])
		{
		    to--;
		    goto done;
		}
	    }
	
    }
    
 done:
    /* Trim trailing slash */
    if (to[0] == '/' && (!absolute || to > path+1))
	to--;

    /* Change the empty string to "." so that stat() on the result
       will always work. */
    if (to == path)
      *to++ = '.';
    
    *to = '\0';

    return;
}

/* It is not clear when this should be used if at all, so I've
   disabled it until someone who understands VMS can look at it. */
#if 0

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
