/* Part of CPP library.  (include file handling)
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1998,
   1999, 2000 Free Software Foundation, Inc.
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
#include "intl.h"

static struct include_hash *include_hash PARAMS ((cpp_reader *,
						  const char *, int));
static struct include_hash *redundant_include_p
					PARAMS ((cpp_reader *,
						 struct include_hash *,
						 struct file_name_list *));
static struct file_name_map *read_name_map
					PARAMS ((cpp_reader *, const char *));
static char *read_filename_string	PARAMS ((int, FILE *));
static char *remap_filename 		PARAMS ((cpp_reader *, char *,
						 struct file_name_list *));
static long read_and_prescan		PARAMS ((cpp_reader *, cpp_buffer *,
						 int, size_t));
static struct file_name_list *actual_directory
					PARAMS ((cpp_reader *, const char *));
static void initialize_input_buffer	PARAMS ((cpp_reader *, int,
						 struct stat *));
static int file_cleanup			PARAMS ((cpp_buffer *, cpp_reader *));
static U_CHAR *find_position		PARAMS ((U_CHAR *, U_CHAR *,
						 unsigned long *));

#if 0
static void hack_vms_include_specification PARAMS ((char *));
#endif

/* Windows does not natively support inodes, and neither does MSDOS.
   Cygwin's emulation can generate non-unique inodes, so don't use it.
   VMS has non-numeric inodes. */
#ifdef VMS
#define INO_T_EQ(a, b) (!bcmp((char *) &(a), (char *) &(b), sizeof (a)))
#elif (defined _WIN32 && ! defined (_UWIN)) \
       || defined __MSDOS__
#define INO_T_EQ(a, b) 0
#else
#define INO_T_EQ(a, b) ((a) == (b))
#endif

#ifndef INCLUDE_LEN_FUDGE
#define INCLUDE_LEN_FUDGE 0
#endif

/* Merge the four include chains together in the order quote, bracket,
   system, after.  Remove duplicate dirs (as determined by
   INO_T_EQ()).  The system_include and after_include chains are never
   referred to again after this function; all access is through the
   bracket_include path.

   For the future: Check if the directory is empty (but
   how?) and possibly preload the include hash. */

void
_cpp_merge_include_chains (opts)
     struct cpp_options *opts;
{
  struct file_name_list *prev, *cur, *other;
  struct file_name_list *quote, *brack, *systm, *after;
  struct file_name_list *qtail, *btail, *stail, *atail;

  qtail = opts->pending->quote_tail;
  btail = opts->pending->brack_tail;
  stail = opts->pending->systm_tail;
  atail = opts->pending->after_tail;

  quote = opts->pending->quote_head;
  brack = opts->pending->brack_head;
  systm = opts->pending->systm_head;
  after = opts->pending->after_head;

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
     -Ibar -I- -Ifoo -Iquux.

     Note that this algorithm is quadratic in the number of -I switches,
     which is acceptable since there aren't usually that many of them.  */

  for (cur = quote, prev = NULL; cur; cur = cur->next)
    {
      for (other = quote; other != cur; other = other->next)
        if (INO_T_EQ (cur->ino, other->ino)
	    && cur->dev == other->dev)
          {
	    if (opts->verbose)
	      fprintf (stderr, _("ignoring duplicate directory `%s'\n"),
		       cur->name);

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
	    if (opts->verbose)
	      fprintf (stderr, _("ignoring duplicate directory `%s'\n"),
		       cur->name);

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
	      if (opts->verbose)
		fprintf (stderr, _("ignoring duplicate directory `%s'\n"),
			 quote->name);

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
	      if (opts->verbose)
		fprintf (stderr, _("ignoring duplicate directory `%s'\n"),
			 qtail->name);

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
}

/* Look up or add an entry to the table of all includes.  This table
 is indexed by the name as it appears in the #include line.  The
 ->next_this_file chain stores all different files with the same
 #include name (there are at least three ways this can happen).  The
 hash function could probably be improved a bit. */

static struct include_hash *
include_hash (pfile, fname, add)
     cpp_reader *pfile;
     const char *fname;
     int add;
{
  unsigned int hash = 0;
  struct include_hash *l, *m;
  const char *f = fname;

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
		     || cpp_defined (pfile, i->control_macro, -1)))
	     ? (struct include_hash *)-1 : i;

  return 0;
}

/* Return 1 if the file named by FNAME has been included before in
   any context, 0 otherwise.  */
int
cpp_included (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  struct include_hash *ptr;

  ptr = include_hash (pfile, fname, 0);
  return (ptr != NULL);
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
_cpp_find_include_file (pfile, fname, search_start, ihash, before)
     cpp_reader *pfile;
     const char *fname;
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
  ih->name = NULL;
  ih->nshort = xstrdup (fname);
  ih->control_macro = NULL;
  
  /* If the pathname is absolute, just open it. */ 
  if (fname[0] == '/')
    {
      ih->foundhere = ABSOLUTE_PATH;
      ih->name = ih->nshort;
      return open (ih->name, O_RDONLY, 0666);
    }

  /* Search directory path, trying to open the file. */

  len = strlen (fname);
  name = xmalloc (len + pfile->max_include_len + 2 + INCLUDE_LEN_FUDGE);

  for (l = search_start; l; l = l->next)
    {
      bcopy (l->name, name, l->nlen);
      name[l->nlen] = '/';
      strcpy (&name[l->nlen+1], fname);
      _cpp_simplify_pathname (name);
      if (CPP_OPTIONS (pfile)->remap)
	name = remap_filename (pfile, name, l);

      f = open (name, O_RDONLY|O_NONBLOCK|O_NOCTTY, 0666);
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
  if (! is_space(ch))
    {
      *set++ = ch;
      while ((ch = getc (f)) != EOF && ! is_space(ch))
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
     const char *dirname;
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
  map_list_ptr->map_list_name = xstrdup (dirname);

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

	  if (is_space(ch))
	    continue;
	  from = read_filename_string (ch, f);
	  while ((ch = getc (f)) != EOF && is_hspace(ch))
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
  const char *from, *p, *dir;

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
      char * newdir = (char *) alloca (p - name + 1);
      bcopy (name, newdir, p - name);
      newdir[p - name] = '\0';
      dir = newdir;
      from = p + 1;
    }
  
  for (map = read_name_map (pfile, dir); map; map = map->map_next)
    if (! strcmp (map->map_from, name))
      return map->map_to;

  return name;
}

/* Push an input buffer and load it up with the contents of FNAME.
   If FNAME is "" or NULL, read standard input.  */
int
cpp_read_file (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  struct include_hash *ih_fake;
  int f;

  if (fname == NULL || *fname == 0)
    {
      fname = "";
      f = 0;
    }

  /* Open the file in nonblocking mode, so we don't get stuck if
     someone clever has asked cpp to process /dev/rmt0.
     _cpp_read_include_file will check that we have a real file to
     work with.  Also take care not to acquire a controlling terminal
     by mistake (this can't happen on sane systems, but paranoia is a
     virtue).  */
  else if ((f = open (fname, O_RDONLY|O_NONBLOCK|O_NOCTTY, 0666)) < 0)
    {
      cpp_notice_from_errno (pfile, fname);
      return 0;
    }

  /* Push the buffer.  */
  if (!cpp_push_buffer (pfile, NULL, 0))
    goto failed_push;
  
  /* Gin up an include_hash structure for this file and feed it
     to finclude.  */

  ih_fake = (struct include_hash *) xmalloc (sizeof (struct include_hash));
  ih_fake->next = 0;
  ih_fake->next_this_file = 0;
  ih_fake->foundhere = ABSOLUTE_PATH;  /* well sort of ... */
  ih_fake->name = fname;
  ih_fake->control_macro = 0;
  ih_fake->buf = (char *)-1;
  ih_fake->limit = 0;
  if (!_cpp_read_include_file (pfile, f, ih_fake))
    goto failed_finclude;

  return 1;

 failed_finclude:
  /* If finclude fails, it pops the buffer.  */
  free (ih_fake);
 failed_push:
  if (f)
    close (f);
  return 0;
}

/* Read the contents of FD into the buffer on the top of PFILE's stack.
   IHASH points to the include hash entry for the file associated with
   FD.

   The caller is responsible for the cpp_push_buffer.  */

int
_cpp_read_include_file (pfile, fd, ihash)
     cpp_reader *pfile;
     int fd;
     struct include_hash *ihash;
{
  struct stat st;
  size_t st_size;
  long length;
  cpp_buffer *fp;

  if (fstat (fd, &st) < 0)
    goto perror_fail;
  if (fcntl (fd, F_SETFL, 0) == -1)  /* turn off nonblocking mode */
    goto perror_fail;

  fp = CPP_BUFFER (pfile);

  /* If fd points to a plain file, we know how big it is, so we can
     allocate the buffer all at once.  If fd is a pipe or terminal, we
     can't.  Most C source files are 4k or less, so we guess that.  If
     fd is something weird, like a block device or a directory, we
     don't want to read it at all.

     Unfortunately, different systems use different st.st_mode values
     for pipes: some have S_ISFIFO, some S_ISSOCK, some are buggy and
     zero the entire struct stat except a couple fields.  Hence the
     mess below.

     In all cases, read_and_prescan will resize the buffer if it
     turns out there's more data than we thought.  */

  if (S_ISREG (st.st_mode))
    {
      /* off_t might have a wider range than size_t - in other words,
	 the max size of a file might be bigger than the address
	 space.  We can't handle a file that large.  (Anyone with
         a single source file bigger than 4GB needs to rethink
	 their coding style.)  */
      st_size = (size_t) st.st_size;
      if ((unsigned HOST_WIDEST_INT) st_size
	  != (unsigned HOST_WIDEST_INT) st.st_size)
	{
	  cpp_error (pfile, "file `%s' is too large", ihash->name);
	  goto fail;
	}
    }
  else if (S_ISFIFO (st.st_mode) || S_ISSOCK (st.st_mode)
	   /* Permit any kind of character device: the sensible ones are
	      ttys and /dev/null, but weeding out the others is too hard.  */
	   || S_ISCHR (st.st_mode)
	   /* Some 4.x (x<4) derivatives have a bug that makes fstat() of a
	      socket or pipe return a stat struct with most fields zeroed.  */
	   || (st.st_mode == 0 && st.st_nlink == 0 && st.st_size == 0))
    {
      /* Cannot get its file size before reading.  4k is a decent
         first guess. */
      st_size = 4096;
    }
  else
    {
      cpp_error (pfile, "`%s' is not a file, pipe, or tty", ihash->name);
      goto fail;
    }

  if (pfile->input_buffer == NULL)
    initialize_input_buffer (pfile, fd, &st);

  /* Read the file, converting end-of-line characters and trigraphs
     (if enabled). */
  fp->ihash = ihash;
  fp->nominal_fname = fp->fname = ihash->name;
  length = read_and_prescan (pfile, fp, fd, st_size);
  if (length < 0)
    goto fail;
  if (length == 0)
    ihash->control_macro = "";  /* never re-include */

  close (fd);
  fp->rlimit = fp->alimit = fp->buf + length;
  fp->cur = fp->buf;
  if (ihash->foundhere != ABSOLUTE_PATH)
      fp->system_header_p = ihash->foundhere->sysp;
  fp->lineno = 1;
  fp->colno = 1;
  fp->line_base = fp->buf;
  fp->cleanup = file_cleanup;

  /* The ->actual_dir field is only used when ignore_srcdir is not in effect;
     see do_include */
  if (!CPP_OPTIONS (pfile)->ignore_srcdir)
    fp->actual_dir = actual_directory (pfile, fp->fname);

  pfile->input_stack_listing_current = 0;
  return 1;

 perror_fail:
  cpp_error_from_errno (pfile, ihash->name);
 fail:
  cpp_pop_buffer (pfile);
  close (fd);
  return 0;
}

/* Given a path FNAME, extract the directory component and place it
   onto the actual_dirs list.  Return a pointer to the allocated
   file_name_list structure.  These structures are used to implement
   current-directory "" include searching. */

static struct file_name_list *
actual_directory (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  char *last_slash, *dir;
  size_t dlen;
  struct file_name_list *x;
  
  dir = xstrdup (fname);
  last_slash = rindex (dir, '/');
  if (last_slash)
    {
      if (last_slash == dir)
        {
	  dlen = 1;
	  last_slash[1] = '\0';
	}
      else
	{
	  dlen = last_slash - dir;
	  *last_slash = '\0';
	}
    }
  else
    {
      dir[0] = '.';
      dir[1] = '\0';
      dlen = 1;
    }

  if (dlen > pfile->max_include_len)
    pfile->max_include_len = dlen;

  for (x = pfile->actual_dirs; x; x = x->alloc)
    if (!strcmp (x->name, dir))
      {
	free (dir);
	return x;
      }

  /* Not found, make a new one. */
  x = (struct file_name_list *) xmalloc (sizeof (struct file_name_list));
  x->name = dir;
  x->nlen = dlen;
  x->next = CPP_OPTIONS (pfile)->quote_include;
  x->alloc = pfile->actual_dirs;
  x->sysp = CPP_BUFFER (pfile)->system_header_p;
  x->name_map = NULL;

  pfile->actual_dirs = x;
  return x;
}

/* Determine the current line and column.  Used only by read_and_prescan. */
static U_CHAR *
find_position (start, limit, linep)
     U_CHAR *start;
     U_CHAR *limit;
     unsigned long *linep;
{
  unsigned long line = *linep;
  U_CHAR *lbase = start;
  while (start < limit)
    {
      U_CHAR ch = *start++;
      if (ch == '\n' || ch == '\r')
	{
	  line++;
	  lbase = start;
	}
    }
  *linep = line;
  return lbase;
}

/* Read the entire contents of file DESC into buffer BUF.  LEN is how
   much memory to allocate initially; more will be allocated if
   necessary.  Convert end-of-line markers (\n, \r, \r\n, \n\r) to
   canonical form (\n).  If enabled, convert and/or warn about
   trigraphs.  Convert backslash-newline to a one-character escape
   (\r) and remove it from "embarrassing" places (i.e. the middle of a
   token).  If there is no newline at the end of the file, add one and
   warn.  Returns -1 on failure, or the actual length of the data to
   be scanned.

   This function does a lot of work, and can be a serious performance
   bottleneck.  It has been tuned heavily; make sure you understand it
   before hacking.  The common case - no trigraphs, Unix style line
   breaks, backslash-newline set off by whitespace, newline at EOF -
   has been optimized at the expense of the others.  The performance
   penalty for DOS style line breaks (\r\n) is about 15%.
   
   Warnings lose particularly heavily since we have to determine the
   line number, which involves scanning from the beginning of the file
   or from the last warning.  The penalty for the absence of a newline
   at the end of reload1.c is about 60%.  (reload1.c is 329k.)

   If your file has more than one kind of end-of-line marker, you
   will get messed-up line numbering.  */

/* Table of characters that can't be handled in the inner loop.
   Keep these contiguous to optimize the performance of the code generated
   for the switch that uses them.  */
#define SPECCASE_EMPTY     0
#define SPECCASE_NUL       1
#define SPECCASE_CR        2
#define SPECCASE_BACKSLASH 3
#define SPECCASE_QUESTION  4

static long
read_and_prescan (pfile, fp, desc, len)
     cpp_reader *pfile;
     cpp_buffer *fp;
     int desc;
     size_t len;
{
  U_CHAR *buf = (U_CHAR *) xmalloc (len);
  U_CHAR *ip, *op, *line_base;
  U_CHAR *ibase;
  U_CHAR *speccase = pfile->input_speccase;
  unsigned long line;
  unsigned int deferred_newlines;
  int count;
  size_t offset;

  offset = 0;
  op = buf;
  line_base = buf;
  line = 1;
  ibase = pfile->input_buffer + 2;
  deferred_newlines = 0;

  for (;;)
    {
    read_next:

      count = read (desc, pfile->input_buffer + 2, pfile->input_buffer_len);
      if (count < 0)
	goto error;
      else if (count == 0)
	break;

      offset += count;
      ip = ibase;
      ibase = pfile->input_buffer + 2;
      ibase[count] = ibase[count+1] = '\0';

      if (offset > len)
	{
	  size_t delta_op;
	  size_t delta_line_base;
	  len *= 2;
	  if (offset > len)
	    /* len overflowed.
	       This could happen if the file is larger than half the
	       maximum address space of the machine. */
	    goto too_big;

	  delta_op = op - buf;
	  delta_line_base = line_base - buf;
	  buf = (U_CHAR *) xrealloc (buf, len);
	  op = buf + delta_op;
	  line_base = buf + delta_line_base;
	}

      for (;;)
	{
	  unsigned int span = 0;

	  /* Deal with \-newline in the middle of a token. */
	  if (deferred_newlines)
	    {
	      while (speccase[ip[span]] == SPECCASE_EMPTY
		     && ip[span] != '\n'
		     && ip[span] != '\t'
		     && ip[span] != ' ')
		span++;
	      memcpy (op, ip, span);
	      op += span;
	      ip += span;
	      /* If ip[0] is SPECCASE_EMPTY, we have hit white space.
		 Dump out the remaining deferred \-newlines.  */
	      if (speccase[ip[0]] == SPECCASE_EMPTY)
		while (deferred_newlines)
		  deferred_newlines--, *op++ = '\r';
	      span = 0;
	    }

	  /* Copy as much as we can without special treatment. */
	  while (speccase[ip[span]] == SPECCASE_EMPTY) span++;
	  memcpy (op, ip, span);
	  op += span;
	  ip += span;

	  switch (speccase[*ip++])
	    {
	    case SPECCASE_NUL:  /* \0 */
	      ibase[-1] = op[-1];
	      goto read_next;

	    case SPECCASE_CR:  /* \r */
	      if (ip[-2] == '\n')
		continue;
	      else if (*ip == '\n')
		ip++;
	      else if (*ip == '\0')
		{
		  *--ibase = '\r';
		  goto read_next;
		}
	      *op++ = '\n';
	      break;

	    case SPECCASE_BACKSLASH:  /* \ */
	    backslash:
	    {
	      /* If we're at the end of the intermediate buffer,
		 we have to shift the backslash down to the start
		 and come back next pass. */
	      if (*ip == '\0')
		{
		  *--ibase = '\\';
		  goto read_next;
		}
	      else if (*ip == '\n')
		{
		  ip++;
		  if (*ip == '\r') ip++;
		  if (*ip == '\n' || *ip == '\t' || *ip == ' ')
		    *op++ = '\r';
		  else if (op[-1] == '\t' || op[-1] == ' '
			   || op[-1] == '\r' || op[-1] == '\n')
		    *op++ = '\r';
		  else
		    deferred_newlines++;
		}
	      else if (*ip == '\r')
		{
		  ip++;
		  if (*ip == '\n') ip++;
		  else if (*ip == '\0')
		    {
		      *--ibase = '\r';
		      *--ibase = '\\';
		      goto read_next;
		    }
		  else if (*ip == '\r' || *ip == '\t' || *ip == ' ')
		    *op++ = '\r';
		  else
		    deferred_newlines++;
		}
	      else
		*op++ = '\\';
	    }
	    break;

	    case SPECCASE_QUESTION: /* ? */
	      {
		unsigned int d, t;
		/* If we're at the end of the intermediate buffer,
		   we have to shift the ?'s down to the start and
		   come back next pass. */
		d = ip[0];
		if (d == '\0')
		  {
		    *--ibase = '?';
		    goto read_next;
		  }
		if (d != '?')
		  {
		    *op++ = '?';
		    break;
		  }
		d = ip[1];
		if (d == '\0')
		  {
		    *--ibase = '?';
		    *--ibase = '?';
		    goto read_next;
		  }

		/* Trigraph map:
		 *	from	to	from	to	from	to
		 *	?? =	#	?? )	]	?? !	|
		 *	?? (	[	?? '	^	?? >	}
		 *	?? /	\	?? <	{	?? -	~
		 */
		if (d == '=') t = '#';
		else if (d == ')') t = ']';
		else if (d == '!') t = '|';
		else if (d == '(') t = '[';
		else if (d == '\'') t = '^';
		else if (d == '>') t = '}';
		else if (d == '/') t = '\\';
		else if (d == '<') t = '{';
		else if (d == '-') t = '~';
		else
		  {
		    *op++ = '?';
		    break;
		  }
		ip += 2;
		if (CPP_OPTIONS (pfile)->warn_trigraphs)
		  {
		    unsigned long col;
		    line_base = find_position (line_base, op, &line);
		    col = op - line_base + 1;
		    if (CPP_OPTIONS (pfile)->trigraphs)
		      cpp_warning_with_line (pfile, line, col,
			     "trigraph ??%c converted to %c", d, t);
		    else
		      cpp_warning_with_line (pfile, line, col,
			     "trigraph ??%c ignored", d);
		  }
		if (CPP_OPTIONS (pfile)->trigraphs)
		  {
		    if (t == '\\')
		      goto backslash;
		    else
		      *op++ = t;
		  }
		else
		  {
		    *op++ = '?';
		    *op++ = '?';
		    *op++ = d;
		  }
	      }
	    }
	}
    }

  if (offset == 0)
    return 0;

  /* Deal with pushed-back chars at true EOF.
     This may be any of:  ?? ? \ \r \n \\r \\n.
     \r must become \n, \\r or \\n must become \r.
     We know we have space already. */
  if (ibase == pfile->input_buffer)
    {
      if (*ibase == '?')
	{
	  *op++ = '?';
	  *op++ = '?';
	}
      else
	*op++ = '\r';
    }
  else if (ibase == pfile->input_buffer + 1)
    {
      if (*ibase == '\r')
	*op++ = '\n';
      else
	*op++ = *ibase;
    }

  if (op[-1] != '\n')
    {
      unsigned long col;
      line_base = find_position (line_base, op, &line);
      col = op - line_base + 1;
      cpp_warning_with_line (pfile, line, col, "no newline at end of file\n");
      if (offset + 1 > len)
	{
	  len += 1;
	  if (offset + 1 > len)
	    goto too_big;
	  buf = (U_CHAR *) xrealloc (buf, len);
	  op = buf + offset;
	}
      *op++ = '\n';
    }

  fp->buf = ((len - offset < 20) ? buf : (U_CHAR *)xrealloc (buf, op - buf));
  return op - buf;

 too_big:
  cpp_error (pfile, "file is too large (>%lu bytes)\n", (unsigned long)offset);
  free (buf);
  return -1;

 error:
  cpp_error_from_errno (pfile, fp->fname);
  free (buf);
  return -1;
}

/* Initialize the `input_buffer' and `input_speccase' tables.
   These are only used by read_and_prescan, but they're large and
   somewhat expensive to set up, so we want them allocated once for
   the duration of the cpp run.  */

static void
initialize_input_buffer (pfile, fd, st)
     cpp_reader *pfile;
     int fd;
     struct stat *st;
{
  long pipe_buf;
  U_CHAR *tmp;

  /* Table of characters that cannot be handled by the
     read_and_prescan inner loop.  The number of non-EMPTY entries
     should be as small as humanly possible.  */

  tmp = (U_CHAR *) xmalloc (1 << CHAR_BIT);
  memset (tmp, SPECCASE_EMPTY, 1 << CHAR_BIT);
  tmp['\0'] = SPECCASE_NUL;
  tmp['\r'] = SPECCASE_CR;
  tmp['\\'] = SPECCASE_BACKSLASH;
  if (CPP_OPTIONS (pfile)->trigraphs || CPP_OPTIONS (pfile)->warn_trigraphs)
    tmp['?'] = SPECCASE_QUESTION;

  pfile->input_speccase = tmp;

  /* Determine the appropriate size for the input buffer.  Normal C
     source files are smaller than eight K.  If we are reading a pipe,
     we want to make sure the input buffer is bigger than the kernel's
     pipe buffer.  */
  pipe_buf = -1;

  if (! S_ISREG (st->st_mode))
    {
#ifdef _PC_PIPE_BUF
      pipe_buf = fpathconf (fd, _PC_PIPE_BUF);
#endif
      if (pipe_buf == -1)
	{
#ifdef PIPE_BUF
	  pipe_buf = PIPE_BUF;
#else
	  pipe_buf = 8192;
#endif
	}
    }

  if (pipe_buf < 8192)
    pipe_buf = 8192;
  /* PIPE_BUF bytes of buffer proper, 2 to detect running off the end
     without address arithmetic all the time, and 2 for pushback in
     the case there's a potential trigraph or end-of-line digraph at
     the end of a block. */

  tmp = (U_CHAR *) xmalloc (pipe_buf + 2 + 2);
  pfile->input_buffer = tmp;
  pfile->input_buffer_len = pipe_buf;
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
void
_cpp_simplify_pathname (path)
    char *path;
{
    char *from, *to;
    char *base;
    int absolute = 0;

#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
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
