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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "cpphash.h"
#include "intl.h"
#include "mkdeps.h"
#include "splay-tree.h"

#ifdef HAVE_MMAP_FILE
# include <sys/mman.h>
# ifndef MMAP_THRESHOLD
#  define MMAP_THRESHOLD 3 /* Minimum page count to mmap the file.  */
# endif

#else  /* No MMAP_FILE */
#  undef MMAP_THRESHOLD
#  define MMAP_THRESHOLD 0
#endif

#ifndef O_BINARY
# define O_BINARY 0
#endif

/* Suppress warning about function macros used w/o arguments in traditional
   C.  It is unlikely that glibc's strcmp macro helps this file at all.  */
#undef strcmp

static struct file_name_map *read_name_map
				PARAMS ((cpp_reader *, const char *));
static char *read_filename_string PARAMS ((int, FILE *));
static char *remap_filename 	PARAMS ((cpp_reader *, char *,
					 struct file_name_list *));
static struct file_name_list *actual_directory
				PARAMS ((cpp_reader *, const char *));
static struct include_file *find_include_file
				PARAMS ((cpp_reader *, const char *,
					 struct file_name_list *));
static struct include_file *open_include_file
				PARAMS ((cpp_reader *, const char *));
static int read_include_file	PARAMS ((cpp_reader *, struct include_file *));
static ssize_t read_with_read	PARAMS ((cpp_buffer *, int, ssize_t));
static ssize_t read_file	PARAMS ((cpp_buffer *, int, ssize_t));

static void destroy_include_file_node	PARAMS ((splay_tree_value));
static int close_cached_fd		PARAMS ((splay_tree_node, void *));
static int report_missing_guard		PARAMS ((splay_tree_node, void *));

#if 0
static void hack_vms_include_specification PARAMS ((char *));
#endif

#ifndef INCLUDE_LEN_FUDGE
#define INCLUDE_LEN_FUDGE 0
#endif

/* We use a splay tree to store information about all the include
   files seen in this compilation.  The key of each tree node is the
   physical path to the file.  The value is 0 if the file does not
   exist, or a struct include_file pointer.  */

static void
destroy_include_file_node (v)
     splay_tree_value v;
{
  struct include_file *f = (struct include_file *)v;
  if (f)
    {
      if (f->fd != -1)
	close (f->fd);
      free (f);
    }
}

static int
close_cached_fd (n, dummy)
     splay_tree_node n;
     void *dummy ATTRIBUTE_UNUSED;
{
  struct include_file *f = (struct include_file *)n->value;
  if (f && f->fd != -1)
    {
      close (f->fd);
      f->fd = -1;
    }
  return 0;
}

void
_cpp_init_includes (pfile)
     cpp_reader *pfile;
{
  pfile->all_include_files
    = splay_tree_new ((splay_tree_compare_fn) strcmp,
		      (splay_tree_delete_key_fn) free,
		      destroy_include_file_node);
}

void
_cpp_cleanup_includes (pfile)
     cpp_reader *pfile;
{
  splay_tree_delete (pfile->all_include_files);
}

/* Given a filename, look it up and possibly open it.  If the file
   does not exist, return NULL.  If the file does exist but doesn't
   need to be reread, return an include_file entry with fd == -1.
   If it needs to be (re)read, return an include_file entry with
   fd a file descriptor open on the file. */

static struct include_file *
open_include_file (pfile, filename)
     cpp_reader *pfile;
     const char *filename;
{
  splay_tree_node nd;
  struct include_file *file = 0;
  int fd;

  nd = splay_tree_lookup (pfile->all_include_files,
			  (splay_tree_key) filename);

  if (nd)
    {
      if (nd->value == 0)
	return 0;

      file = (struct include_file *)nd->value;

      if (DO_NOT_REREAD (file))
	{
	  if (file->fd != -1)
	    {
	      close (file->fd);
	      file->fd = -1;
	    }
	  return file;
	}

      /* File descriptors are cached for files that might be reread.  */
      if (file->fd != -1)
	{
	  lseek (file->fd, 0, SEEK_SET);
	  return file;
	}
    }

  /* We used to open files in nonblocking mode, but that caused more
     problems than it solved.  Do take care not to acquire a
     controlling terminal by mistake (this can't happen on sane
     systems, but paranoia is a virtue).

     Use the three-argument form of open even though we aren't
     specifying O_CREAT, to defend against broken system headers.

     O_BINARY tells some runtime libraries (notably DJGPP) not to do
     newline translation; we can handle DOS line breaks just fine
     ourselves.

     Special case: the empty string is translated to stdin.  */
 retry:

  if (filename[0] == '\0')
    fd = 0;
  else
    fd = open (filename, O_RDONLY|O_NOCTTY|O_BINARY, 0666);

  if (fd == -1)
    {
#ifdef EACCES
      if (errno == EACCES)
	{
	  cpp_error (pfile, "included file \"%s\" exists but is not readable",
		     filename);
	}
#endif
      if (0
#ifdef EMFILE
	  || errno == EMFILE
#endif
#ifdef ENFILE
	  || errno == ENFILE
#endif
	  )
	{
	  /* Too many files open.  Close all cached file descriptors and
	     try again.  */
	  splay_tree_foreach (pfile->all_include_files, close_cached_fd, 0);
	  goto retry;
	}

      /* Nonexistent or inaccessible file.  Create a negative node for it.  */
      if (nd)
	{
	  cpp_ice (pfile,
		   "node for '%s' exists, open failed, error '%s', value %lx\n",
		   filename, strerror (errno), nd->value);
	  destroy_include_file_node (nd->value);
	}
      splay_tree_insert (pfile->all_include_files,
			 (splay_tree_key) xstrdup (filename), 0);
      return 0;
    }

  /* If we haven't seen this file before, create a positive node for it.  */
  if (!nd)
    {
      file = xnew (struct include_file);
      file->cmacro = 0;
      file->include_count = 0;
      file->sysp = 0;
      file->foundhere = 0;
      file->name = xstrdup (filename);
      splay_tree_insert (pfile->all_include_files,
			 (splay_tree_key) file->name,
			 (splay_tree_value) file);
    }

  file->fd = fd;
  file->date = (time_t) -1;
  return file;
}

/* Return 1 if the file named by FNAME has been included before in
   any context, 0 otherwise.  */
int
cpp_included (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  struct file_name_list *path;
  char *name;
  splay_tree_node nd;

  if (fname[0] == '/')
    {
      /* Just look it up.  */
      nd = splay_tree_lookup (pfile->all_include_files, (splay_tree_key) fname);
      return (nd && nd->value);
    }
      
  /* Search directory path for the file.  */
  name = (char *) alloca (strlen (fname) + pfile->max_include_len
			  + 2 + INCLUDE_LEN_FUDGE);
  for (path = CPP_OPTION (pfile, quote_include); path; path = path->next)
    {
      memcpy (name, path->name, path->nlen);
      name[path->nlen] = '/';
      strcpy (&name[path->nlen+1], fname);
      _cpp_simplify_pathname (name);
      if (CPP_OPTION (pfile, remap))
	name = remap_filename (pfile, name, path);

      nd = splay_tree_lookup (pfile->all_include_files, (splay_tree_key) name);
      if (nd && nd->value)
	return 1;
    }
  return 0;
}

/* Search for include file FNAME in the include chain starting at
   SEARCH_START.  Return 0 if there is no such file (or it's un-openable),
   otherwise an include_file structure, possibly with a file descriptor
   open on the file.  */

static struct include_file *
find_include_file (pfile, fname, search_start)
     cpp_reader *pfile;
     const char *fname;
     struct file_name_list *search_start;
{
  struct file_name_list *path;
  char *name;
  struct include_file *file;

  if (fname[0] == '/')
    return open_include_file (pfile, fname);
      
  /* Search directory path for the file.  */
  name = (char *) alloca (strlen (fname) + pfile->max_include_len
			  + 2 + INCLUDE_LEN_FUDGE);
  for (path = search_start; path; path = path->next)
    {
      memcpy (name, path->name, path->nlen);
      name[path->nlen] = '/';
      strcpy (&name[path->nlen+1], fname);
      _cpp_simplify_pathname (name);
      if (CPP_OPTION (pfile, remap))
	name = remap_filename (pfile, name, path);

      file = open_include_file (pfile, name);
      if (file)
	{
	  file->sysp = path->sysp;
	  file->foundhere = path;
	  return file;
	}
    }
  return 0;
}

/* #line uses this to save artificial file names.  We have to try
   opening the file because an all_include_files entry is always
   either + or -, there's no 'I don't know' value.  */
const char *
_cpp_fake_include (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  splay_tree_node nd;
  struct include_file *file;
  char *name;

  file = find_include_file (pfile, fname, CPP_OPTION (pfile, quote_include));
  if (file)
    return file->name;

  name = xstrdup (fname);
  _cpp_simplify_pathname (name);

  /* We cannot just blindly insert a node, because there's still the
     chance that the node already exists but isn't on the search path.  */
  nd = splay_tree_lookup (pfile->all_include_files, (splay_tree_key) name);
  if (nd)
    {
      free (name);
      return (const char *) nd->key;
    }

  splay_tree_insert (pfile->all_include_files, (splay_tree_key) name, 0);
  return (const char *)name;
}

/* Not everyone who wants to set system-header-ness on a buffer can
   see the details of struct include_file.  This is an exported interface
   because fix-header needs it.  */
void
cpp_make_system_header (pfile, pbuf, flag)
     cpp_reader *pfile;
     cpp_buffer *pbuf;
     int flag;
{
  if (flag < 0 || flag > 2)
    cpp_ice (pfile, "cpp_make_system_header: bad flag %d\n", flag);
  else if (!pbuf->inc)
    cpp_ice (pfile, "cpp_make_system_header called on non-file buffer");
  else
    pbuf->inc->sysp = flag;
}

/* Report on all files that might benefit from a multiple include guard.
   Triggered by -H.  */
void
_cpp_report_missing_guards (pfile)
     cpp_reader *pfile;
{
  int banner = 0;
  splay_tree_foreach (pfile->all_include_files, report_missing_guard,
		      (PTR) &banner);
}

static int
report_missing_guard (n, b)
     splay_tree_node n;
     void *b;
{
  struct include_file *f = (struct include_file *) n->value;
  int *bannerp = (int *)b;

  if (f && f->cmacro == 0 && f->include_count == 1)
    {
      if (*bannerp == 0)
	{
	  fputs (_("Multiple include guards may be useful for:\n"), stderr);
	  *bannerp = 1;
	}
      fputs (f->name, stderr);
      putc ('\n', stderr);
    }
  return 0;
}

#define PRINT_THIS_DEP(p, b) (CPP_PRINT_DEPS(p) > (b||p->system_include_depth))
void
_cpp_execute_include (pfile, f, len, no_reinclude, search_start, angle_brackets)
     cpp_reader *pfile;
     const U_CHAR *f;
     unsigned int len;
     int no_reinclude;
     struct file_name_list *search_start;
     int angle_brackets;
{
  struct include_file *inc;
  char *fname;

  if (!search_start)
    {
      if (angle_brackets)
	search_start = CPP_OPTION (pfile, bracket_include);
      else if (CPP_OPTION (pfile, ignore_srcdir))
	search_start = CPP_OPTION (pfile, quote_include);
      else
	search_start = CPP_BUFFER (pfile)->actual_dir;
    }

  if (!search_start)
    {
      cpp_error (pfile, "No include path in which to find %s", f);
      return;
    }

  fname = alloca (len + 1);
  memcpy (fname, f, len);
  fname[len] = '\0';

  inc = find_include_file (pfile, fname, search_start);

  if (inc)
    {
      if (inc->fd == -1)
	return;

      /* For -M, add the file to the dependencies on its first inclusion. */
      if (!inc->include_count && PRINT_THIS_DEP (pfile, angle_brackets))
	deps_add_dep (pfile->deps, inc->name);
      inc->include_count++;

      /* Handle -H option.  */
      if (CPP_OPTION (pfile, print_include_names))
	{
	  cpp_buffer *fp = CPP_BUFFER (pfile);
	  while ((fp = CPP_PREV_BUFFER (fp)) != NULL)
	    putc ('.', stderr);
	  fprintf (stderr, " %s\n", inc->name);
	}

      /* Actually process the file.  */
      if (no_reinclude)
	inc->cmacro = NEVER_REREAD;
  
      if (read_include_file (pfile, inc))
	{
	  if (angle_brackets)
	    pfile->system_include_depth++;
	}
      return;
    }
      
  if (CPP_OPTION (pfile, print_deps_missing_files)
      && PRINT_THIS_DEP (pfile, angle_brackets))
    {
      if (!angle_brackets)
	deps_add_dep (pfile->deps, fname);
      else
	{
	  char *p;
	  struct file_name_list *ptr;
	  /* If requested as a system header, assume it belongs in
	     the first system header directory. */
	  if (CPP_OPTION (pfile, bracket_include))
	    ptr = CPP_OPTION (pfile, bracket_include);
	  else
	    ptr = CPP_OPTION (pfile, quote_include);

	  p = (char *) alloca (strlen (ptr->name)
			       + strlen (fname) + 2);
	  if (*ptr->name != '\0')
	    {
	      strcpy (p, ptr->name);
	      strcat (p, "/");
	    }
	  strcat (p, fname);
	  _cpp_simplify_pathname (p);
	  deps_add_dep (pfile->deps, p);
	}
    }
  /* If -M was specified, and this header file won't be added to
     the dependency list, then don't count this as an error,
     because we can still produce correct output.  Otherwise, we
     can't produce correct output, because there may be
     dependencies we need inside the missing file, and we don't
     know what directory this missing file exists in. */
  else if (CPP_PRINT_DEPS (pfile)
	   && ! PRINT_THIS_DEP (pfile, angle_brackets))
    cpp_warning (pfile, "No include path in which to find %s", fname);
  else
    cpp_error_from_errno (pfile, fname);
}

/* Locate file F, and determine whether it is newer than PFILE. Return -1,
   if F cannot be located or dated, 1, if it is newer and 0 if older.  */

int
_cpp_compare_file_date (pfile, f, len, angle_brackets)
     cpp_reader *pfile;
     const U_CHAR *f;
     unsigned int len;
     int angle_brackets;
{
  char *fname;
  struct file_name_list *search_start;
  struct include_file *inc;
  struct include_file *current_include = CPP_BUFFER (pfile)->inc;

  if (angle_brackets)
    search_start = CPP_OPTION (pfile, bracket_include);
  else if (CPP_OPTION (pfile, ignore_srcdir))
    search_start = CPP_OPTION (pfile, quote_include);
  else
    search_start = CPP_BUFFER (pfile)->actual_dir;

  fname = alloca (len + 1);
  memcpy (fname, f, len);
  fname[len] = '\0';
  inc = find_include_file (pfile, fname, search_start);
  
  if (!inc)
    return -1;
  if (inc->fd >= 0)
    {
      struct stat source;
      
      if (fstat (inc->fd, &source) < 0)
        {
          close (inc->fd);
          inc->fd = -1;
          return -1;
        }
      inc->date = source.st_mtime;
      close (inc->fd);
      inc->fd = -1;
    }
  if (inc->date == (time_t)-1 || current_include->date == (time_t)-1)
    return -1;
  return inc->date > current_include->date;
}


/* Push an input buffer and load it up with the contents of FNAME.
   If FNAME is "" or NULL, read standard input.  */
int
cpp_read_file (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  struct include_file *f;

  if (fname == NULL)
    fname = "";

  f = open_include_file (pfile, fname);

  if (f == NULL)
    {
      cpp_error_from_errno (pfile, fname);
      return 0;
    }

  return read_include_file (pfile, f);
}

/* Read the file referenced by INC into a new buffer on PFILE's stack.
   Return 1 if successful, 0 if not.  */

static int
read_include_file (pfile, inc)
     cpp_reader *pfile;
     struct include_file *inc;
{
  struct stat st;
  ssize_t length;
  cpp_buffer *fp;
  int fd = inc->fd;

  /* Ensures we dump our current line before entering an include file.  */
  if (CPP_BUFFER (pfile) && pfile->printer)
    cpp_output_tokens (pfile, pfile->printer,
		       CPP_BUF_LINE (CPP_BUFFER (pfile)));

  fp = cpp_push_buffer (pfile, NULL, 0);

  if (fp == 0)
    goto push_fail;

  if (fd < 0 || fstat (fd, &st) < 0)
    goto perror_fail;
  
  inc->date = st.st_mtime;

  /* If fd points to a plain file, we might be able to mmap it; we can
     definitely allocate the buffer all at once.  If fd is a pipe or
     terminal, we can't do either.  If fd is something weird, like a
     block device or a directory, we don't want to read it at all.

     Unfortunately, different systems use different st.st_mode values
     for pipes: some have S_ISFIFO, some S_ISSOCK, some are buggy and
     zero the entire struct stat except a couple fields.  Hence we don't
     even try to figure out what something is, except for plain files,
     directories, and block devices.  */

  if (S_ISREG (st.st_mode))
    {
      ssize_t st_size;

      /* off_t might have a wider range than ssize_t - in other words,
	 the max size of a file might be bigger than the address
	 space.  We can't handle a file that large.  (Anyone with
	 a single source file bigger than 2GB needs to rethink
	 their coding style.)  Some systems (e.g. AIX 4.1) define
	 SSIZE_MAX to be much smaller than the actual range of the
	 type.  Use INTTYPE_MAXIMUM unconditionally to ensure this
	 does not bite us.  */
      if (st.st_size > INTTYPE_MAXIMUM (ssize_t))
	{
	  cpp_error (pfile, "%s is too large", inc->name);
	  goto fail;
	}
      st_size = st.st_size;
      length = read_file (fp, fd, st_size);
      if (length == -1)
	goto perror_fail;
      if (length < st_size)
	cpp_warning (pfile, "%s is shorter than expected\n", inc->name);
    }
  else if (S_ISBLK (st.st_mode))
    {
      cpp_error (pfile, "%s is a block device", inc->name);
      goto fail;
    }
  else if (S_ISDIR (st.st_mode))
    {
      cpp_error (pfile, "%s is a directory", inc->name);
      goto fail;
    }
  else
    {
      /* 8 kilobytes is a sensible starting size.  It ought to be
	 bigger than the kernel pipe buffer, and it's definitely
	 bigger than the majority of C source files.  */
      length = read_with_read (fp, fd, 8 * 1024);
      if (length == -1)
	goto perror_fail;
    }

  /* These must be set before prescan.  */
  fp->inc = inc;
  fp->nominal_fname = inc->name;
  pfile->include_depth++;
  
  if (length == 0)
    inc->cmacro = NEVER_REREAD;

  fp->rlimit = fp->buf + length;
  fp->cur = fp->buf;
  fp->lineno = 1;
  fp->line_base = fp->buf;

  /* The ->actual_dir field is only used when ignore_srcdir is not in effect;
     see do_include */
  if (!CPP_OPTION (pfile, ignore_srcdir))
    fp->actual_dir = actual_directory (pfile, inc->name);

  pfile->input_stack_listing_current = 0;
  return 1;

 perror_fail:
  cpp_error_from_errno (pfile, inc->name);
  /* Do not try to read this file again.  */
  if (fd != -1)
    close (fd);
  inc->fd = -1;
  inc->cmacro = NEVER_REREAD;
 fail:
  cpp_pop_buffer (pfile);
 push_fail:
  return 0;
}

static ssize_t
read_file (fp, fd, size)
     cpp_buffer *fp;
     int fd;
     ssize_t size;
{
  static int pagesize = -1;

  if (size == 0)
    return 0;

  if (pagesize == -1)
    pagesize = getpagesize ();

#if MMAP_THRESHOLD
  if (size / pagesize >= MMAP_THRESHOLD)
    {
      const U_CHAR *result
	= (const U_CHAR *) mmap (0, size, PROT_READ, MAP_PRIVATE, fd, 0);
      if (result != (const U_CHAR *)-1)
	{
	  fp->buf = result;
	  fp->mapped = 1;
	  return size;
	}
    }
  /* If mmap fails, try read.  If there's really a problem, read will
     fail too.  */
#endif

  return read_with_read (fp, fd, size);
}

static ssize_t
read_with_read (fp, fd, size)
     cpp_buffer *fp;
     int fd;
     ssize_t size;
{
  ssize_t offset, count;
  U_CHAR *buf;

  buf = (U_CHAR *) xmalloc (size);
  offset = 0;
  while ((count = read (fd, buf + offset, size - offset)) > 0)
    {
      offset += count;
      if (offset == size)
	buf = xrealloc (buf, (size *= 2));
    }
  if (count < 0)
    {
      free (buf);
      return -1;
    }
  if (offset == 0)
    {
      free (buf);
      return 0;
    }

  if (offset < size)
    buf = xrealloc (buf, offset);
  fp->buf = buf;
  fp->mapped = 0;
  return offset;
}

/* Do appropriate cleanup when a file buffer is popped off the input
   stack.  */
void
_cpp_pop_file_buffer (pfile, buf)
     cpp_reader *pfile;
     cpp_buffer *buf;
{
  struct include_file *inc = buf->inc;

  if (pfile->system_include_depth)
    pfile->system_include_depth--;
  if (pfile->include_depth)
    pfile->include_depth--;
  if (pfile->potential_control_macro)
    {
      if (inc->cmacro != NEVER_REREAD)
	inc->cmacro = pfile->potential_control_macro;
      pfile->potential_control_macro = 0;
    }
  pfile->input_stack_listing_current = 0;

  /* Discard file buffer.  XXX Would be better to cache these instead
     of the file descriptors.  */
#ifdef HAVE_MMAP_FILE
  if (buf->mapped)
    munmap ((caddr_t) buf->buf, buf->rlimit - buf->buf);
  else
#endif
    free ((PTR) buf->buf);

  /* If the file will not be included again, close it.  */
  if (DO_NOT_REREAD (inc))
    {
      close (inc->fd);
      inc->fd = -1;
    }
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

  for (map_list_ptr = CPP_OPTION (pfile, map_list); map_list_ptr;
       map_list_ptr = map_list_ptr->map_list_next)
    if (! strcmp (map_list_ptr->map_list_name, dirname))
      return map_list_ptr->map_list_map;

  map_list_ptr = ((struct file_name_map_list *)
		  xmalloc (sizeof (struct file_name_map_list)));
  map_list_ptr->map_list_name = xstrdup (dirname);
  map_list_ptr->map_list_map = NULL;

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
  
  map_list_ptr->map_list_next = CPP_OPTION (pfile, map_list);
  CPP_OPTION (pfile, map_list) = map_list_ptr;

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
  p = strrchr (name, '/');
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
      memcpy (newdir, name, p - name);
      newdir[p - name] = '\0';
      dir = newdir;
      from = p + 1;
    }
  
  for (map = read_name_map (pfile, dir); map; map = map->map_next)
    if (! strcmp (map->map_from, name))
      return map->map_to;

  return name;
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
  last_slash = strrchr (dir, '/');
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
  x->next = CPP_OPTION (pfile, quote_include);
  x->alloc = pfile->actual_dirs;
  x->sysp = CPP_BUFFER (pfile)->inc->sysp;
  x->name_map = NULL;

  pfile->actual_dirs = x;
  return x;
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
  first_slash = strchr (fullname, '/');
  if (first_slash == 0)
    return 0;				/* Nothing to do!!! */

  /* construct device spec if none given.  */

  if (strchr (fullname, ':') == 0)
    {

      /* If fullname has a slash, take it as device spec.  */

      if (first_slash == fullname)
	{
	  first_slash = strchr (fullname + 1, '/');	/* 2nd slash ? */
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
	   usual way.  Given the default locations for include files,
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

  if (strchr (unixname, '/') == 0)
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
      while (strchr (unixname, '/') != 0)
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
      f = open (fullname, O_RDONLY|O_NONBLOCK);
      if (f >= 0)
	{
	  /* The file name is OK as it is, so return it as is.  */
	  close (f);
	  return 1;
	}

      /* The filename did not work.  Try to remove the [000000] from the name,
	 and return it.  */

      basename = strchr (fullname, '[');
      local_ptr = strchr (fullname, ']') + 1;
      strcpy (basename, local_ptr);		/* this gets rid of it */

    }

  return 1;
}
#endif	/* VMS */
