/* Part of CPP library.  (include file handling)
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.
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

#ifndef INCLUDE_LEN_FUDGE
# define INCLUDE_LEN_FUDGE 0
#endif

/* If errno is inspected immediately after a system call fails, it will be
   nonzero, and no error number will ever be zero.  */
#ifndef ENOENT
# define ENOENT 0
#endif
#ifndef ENOTDIR
# define ENOTDIR 0
#endif
#ifndef ENOMEM
# define ENOMEM 0
#endif

/* Suppress warning about function macros used w/o arguments in traditional
   C.  It is unlikely that glibc's strcmp macro helps this file at all.  */
#undef strcmp

/* This structure is used for the table of all includes.  */
struct include_file
{
  const char *name;		/* actual path name of file */
  const cpp_hashnode *cmacro;	/* macro, if any, preventing reinclusion.  */
  const struct search_path *foundhere;
				/* location in search path where file was
				   found, for #include_next and sysp.  */
  const unsigned char *buffer;	/* pointer to cached file contents */
  struct stat st;		/* copy of stat(2) data for file */
  int fd;			/* fd open on file (short term storage only) */
  unsigned short include_count;	/* number of times file has been read */
  unsigned short refcnt;	/* number of stacked buffers using this file */
  unsigned char mapped;		/* file buffer is mmapped */
  unsigned char defined;	/* cmacro prevents inclusion in this state */
};

/* The cmacro works like this: If it's NULL, the file is to be
   included again.  If it's NEVER_REREAD, the file is never to be
   included again.  Otherwise it is a macro hashnode, and the file is
   to be included again if the macro is defined or not as specified by
   DEFINED.  */
#define NEVER_REREAD ((const cpp_hashnode *)-1)
#define DO_NOT_REREAD(inc) \
((inc)->cmacro && ((inc)->cmacro == NEVER_REREAD \
		   || ((inc)->cmacro->type == NT_MACRO) == (inc)->defined))

static struct file_name_map *read_name_map
				PARAMS ((cpp_reader *, const char *));
static char *read_filename_string PARAMS ((int, FILE *));
static char *remap_filename 	PARAMS ((cpp_reader *, char *,
					 struct search_path *));
static struct search_path *search_from PARAMS ((cpp_reader *,
						struct include_file *));
static struct include_file *find_include_file
				PARAMS ((cpp_reader *, const char *,
					 struct search_path *));
static struct include_file *open_file PARAMS ((cpp_reader *, const char *));
static void read_include_file	PARAMS ((cpp_reader *, struct include_file *));
static void stack_include_file	PARAMS ((cpp_reader *, struct include_file *));
static void purge_cache 	PARAMS ((struct include_file *));
static void destroy_include_file_node	PARAMS ((splay_tree_value));
static int report_missing_guard		PARAMS ((splay_tree_node, void *));

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
      purge_cache (f);
      free (f);  /* The tree is registered with free to free f->name.  */
    }
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

/* Mark a file to not be reread (e.g. #import, read failure).  */
void
_cpp_never_reread (file)
     struct include_file *file;
{
  file->cmacro = NEVER_REREAD;
}

/* Put a file name in the splay tree, for the sake of cpp_included ().
   Assume that FNAME has already had its path simplified.  */
void
_cpp_fake_include (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  splay_tree_node nd;

  nd = splay_tree_lookup (pfile->all_include_files, (splay_tree_key) fname);
  if (! nd)
    {
      struct include_file *file = xcnew (struct include_file);
      file->name = xstrdup (fname);
      splay_tree_insert (pfile->all_include_files,
			 (splay_tree_key) file->name,
			 (splay_tree_value) file);
    }
}

/* Given a file name, look it up in the cache; if there is no entry,
   create one with a non-NULL value (regardless of success in opening
   the file).  If the file doesn't exist or is inaccessible, this
   entry is flagged so we don't attempt to open it again in the
   future.  If the file isn't open, open it.  The empty string is
   interpreted as stdin.

   Returns an include_file structure with an open file descriptor on
   success, or NULL on failure.  */

static struct include_file *
open_file (pfile, filename)
     cpp_reader *pfile;
     const char *filename;
{
  splay_tree_node nd;
  struct include_file *file;

  nd = splay_tree_lookup (pfile->all_include_files, (splay_tree_key) filename);

  if (nd)
    {
      file = (struct include_file *) nd->value;

      /* Don't retry opening if we failed previously.  */
      if (file->fd == -2)
	return 0;

      /* Don't reopen an idempotent file. */
      if (DO_NOT_REREAD (file))
        return file;
      
      /* Don't reopen one which is already loaded. */
      if (file->buffer != NULL)
        return file;
    }
  else
    {
      /* In particular, this clears foundhere.  */
      file = xcnew (struct include_file);
      file->name = xstrdup (filename);
      splay_tree_insert (pfile->all_include_files,
			 (splay_tree_key) file->name,
			 (splay_tree_value) file);
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

  if (filename[0] == '\0')
    file->fd = 0;
  else
    file->fd = open (filename, O_RDONLY | O_NOCTTY | O_BINARY, 0666);

  if (file->fd != -1 && fstat (file->fd, &file->st) == 0)
    {
      /* Mark a regular, zero-length file never-reread now.  */
      if (S_ISREG (file->st.st_mode) && file->st.st_size == 0)
        {
	  _cpp_never_reread (file);
	  close (file->fd);
	  file->fd = -1;
	}

      return file;
    }

  /* Don't issue an error message if the file doesn't exist.  */
  if (errno != ENOENT && errno != ENOTDIR)
    cpp_error_from_errno (pfile, filename);

  /* Create a negative node for this path, and return null.  */
  file->fd = -2;

  return 0;
}

/* Place the file referenced by INC into a new buffer on PFILE's
   stack.  If there are errors, or the file should not be re-included,
   a null buffer is pushed.  */

static void
stack_include_file (pfile, inc)
     cpp_reader *pfile;
     struct include_file *inc;
{
  size_t len = 0;
  cpp_buffer *fp;
  int sysp, deps_sysp;

  /* We'll try removing deps_sysp after the release of 3.0.  */
  deps_sysp = pfile->system_include_depth != 0;
  sysp = ((pfile->buffer && pfile->buffer->sysp)
	  || (inc->foundhere && inc->foundhere->sysp));

  /* For -M, add the file to the dependencies on its first inclusion.  */
  if (CPP_OPTION (pfile, print_deps) > deps_sysp && !inc->include_count)
    deps_add_dep (pfile->deps, inc->name);

  /* We don't want multiple include guard advice for the main file.  */
  if (pfile->buffer)
    inc->include_count++;

  /* Not in cache?  */
  if (! inc->buffer)
    read_include_file (pfile, inc);

  if (! DO_NOT_REREAD (inc))
    len = inc->st.st_size;

  /* Push a buffer.  */
  fp = cpp_push_buffer (pfile, inc->buffer, len, BUF_FILE, inc->name);
  fp->inc = inc;
  fp->inc->refcnt++;
  fp->sysp = sysp;
  fp->search_from = search_from (pfile, inc);

  /* Initialise controlling macro state.  */
  pfile->mi_state = MI_OUTSIDE;
  pfile->mi_cmacro = 0;
  pfile->include_depth++;

  /* Generate the call back.  */
  fp->lineno = 0;
  _cpp_do_file_change (pfile, FC_ENTER, 0, 0);
  fp->lineno = 1;
}

/* Read the file referenced by INC into the file cache.

   If fd points to a plain file, we might be able to mmap it; we can
   definitely allocate the buffer all at once.  If fd is a pipe or
   terminal, we can't do either.  If fd is something weird, like a
   block device or a directory, we don't want to read it at all.

   Unfortunately, different systems use different st.st_mode values
   for pipes: some have S_ISFIFO, some S_ISSOCK, some are buggy and
   zero the entire struct stat except a couple fields.  Hence we don't
   even try to figure out what something is, except for plain files,
   directories, and block devices.

   FIXME: Flush file cache and try again if we run out of memory.  */

static void
read_include_file (pfile, inc)
     cpp_reader *pfile;
     struct include_file *inc;
{
  ssize_t size, offset, count;
  U_CHAR *buf;
#if MMAP_THRESHOLD
  static int pagesize = -1;
#endif

  if (DO_NOT_REREAD (inc))
    return;

  if (S_ISREG (inc->st.st_mode))
    {
      /* off_t might have a wider range than ssize_t - in other words,
	 the max size of a file might be bigger than the address
	 space.  We can't handle a file that large.  (Anyone with
	 a single source file bigger than 2GB needs to rethink
	 their coding style.)  Some systems (e.g. AIX 4.1) define
	 SSIZE_MAX to be much smaller than the actual range of the
	 type.  Use INTTYPE_MAXIMUM unconditionally to ensure this
	 does not bite us.  */
      if (inc->st.st_size > INTTYPE_MAXIMUM (ssize_t))
	{
	  cpp_error (pfile, "%s is too large", inc->name);
	  goto fail;
	}
      size = inc->st.st_size;

      inc->mapped = 0;
#if MMAP_THRESHOLD
      if (pagesize == -1)
	pagesize = getpagesize ();

      if (size / pagesize >= MMAP_THRESHOLD)
	{
	  buf = (U_CHAR *) mmap (0, size, PROT_READ, MAP_PRIVATE, inc->fd, 0);
	  if (buf == (U_CHAR *)-1)
	    goto perror_fail;
	  inc->mapped = 1;
	}
      else
#endif
	{
	  buf = (U_CHAR *) xmalloc (size);
	  offset = 0;
	  while (offset < size)
	    {
	      count = read (inc->fd, buf + offset, size - offset);
	      if (count < 0)
		goto perror_fail;
	      if (count == 0)
		{
		  cpp_warning (pfile, "%s is shorter than expected", inc->name);
		  break;
		}
	      offset += count;
	    }
	}
    }
  else if (S_ISBLK (inc->st.st_mode))
    {
      cpp_error (pfile, "%s is a block device", inc->name);
      goto fail;
    }
  else if (S_ISDIR (inc->st.st_mode))
    {
      cpp_error (pfile, "%s is a directory", inc->name);
      goto fail;
    }
  else
    {
      /* 8 kilobytes is a sensible starting size.  It ought to be
	 bigger than the kernel pipe buffer, and it's definitely
	 bigger than the majority of C source files.  */
      size = 8 * 1024;

      buf = (U_CHAR *) xmalloc (size);
      offset = 0;
      while ((count = read (inc->fd, buf + offset, size - offset)) > 0)
	{
	  offset += count;
	  if (offset == size)
	    buf = xrealloc (buf, (size *= 2));
	}
      if (count < 0)
	goto perror_fail;

      if (offset < size)
	buf = xrealloc (buf, offset);
      inc->st.st_size = offset;
    }

  close (inc->fd);
  inc->buffer = buf;
  inc->fd = -1;
  return;

 perror_fail:
  cpp_error_from_errno (pfile, inc->name);
 fail:
  /* Do not try to read this file again.  */
  close (inc->fd);
  inc->fd = -1;
  _cpp_never_reread (inc);
  return;
}

static void
purge_cache (inc)
     struct include_file *inc;
{
  if (inc->buffer)
    {
#if MMAP_THRESHOLD
      if (inc->mapped)
	munmap ((PTR) inc->buffer, inc->st.st_size);
      else
#endif
	free ((PTR) inc->buffer);
      inc->buffer = NULL;
    }
}

/* Return 1 if the file named by FNAME has been included before in
   any context, 0 otherwise.  */
int
cpp_included (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  struct search_path *path;
  char *name;
  splay_tree_node nd;

  if (IS_ABSOLUTE_PATHNAME (fname))
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
      memcpy (name, path->name, path->len);
      name[path->len] = '/';
      strcpy (&name[path->len + 1], fname);
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
   otherwise an include_file structure.  */

static struct include_file *
find_include_file (pfile, fname, search_start)
     cpp_reader *pfile;
     const char *fname;
     struct search_path *search_start;
{
  struct search_path *path;
  char *name;
  struct include_file *file;

  if (IS_ABSOLUTE_PATHNAME (fname))
    return open_file (pfile, fname);
      
  /* Search directory path for the file.  */
  name = (char *) alloca (strlen (fname) + pfile->max_include_len
			  + 2 + INCLUDE_LEN_FUDGE);
  for (path = search_start; path; path = path->next)
    {
      memcpy (name, path->name, path->len);
      name[path->len] = '/';
      strcpy (&name[path->len + 1], fname);
      _cpp_simplify_pathname (name);
      if (CPP_OPTION (pfile, remap))
	name = remap_filename (pfile, name, path);

      file = open_file (pfile, name);
      if (file)
	{
	  file->foundhere = path;
	  return file;
	}
    }

  return 0;
}

/* Not everyone who wants to set system-header-ness on a buffer can
   see the details of a buffer.  This is an exported interface because
   fix-header needs it.  */
void
cpp_make_system_header (pfile, syshdr, externc)
     cpp_reader *pfile;
     int syshdr, externc;
{
  int flags = 0;

  /* 1 = system header, 2 = system header to be treated as C.  */
  if (syshdr)
    flags = 1 + (externc != 0);
  pfile->buffer->sysp = flags;
  _cpp_do_file_change (pfile, FC_RENAME, pfile->buffer->nominal_fname,
		       pfile->buffer->lineno);
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

void
_cpp_execute_include (pfile, header, no_reinclude, include_next)
     cpp_reader *pfile;
     const cpp_token *header;
     int no_reinclude;
     int include_next;
{
  struct search_path *search_start = 0;
  unsigned int angle_brackets = header->type == CPP_HEADER_NAME;
  const char *fname = (const char *) header->val.str.text;
  struct include_file *inc;
  int print_dep;

  /* Help protect #include or similar from recursion.  */
  if (pfile->buffer_stack_depth >= CPP_STACK_MAX)
    {
      cpp_fatal (pfile, "#include nested too deeply");
      return;
    }

  /* Check we've tidied up #include before entering the buffer.  */
  if (pfile->context->prev)
    {
      cpp_ice (pfile, "attempt to push file buffer with contexts stacked");
      return;
    }

  /* For #include_next, skip in the search path past the dir in which
     the current file was found.  If this is the last directory in the
     search path, don't include anything.  If the current file was
     specified with an absolute path, use the normal search logic.  If
     this is the primary source file, use the normal search logic and
     generate a warning.  */
  if (include_next)
    {
      if (! pfile->buffer->prev)
	cpp_warning (pfile, "#include_next in primary source file");
      else
	{
	  if (pfile->buffer->inc->foundhere)
	    {
	      search_start = pfile->buffer->inc->foundhere->next;
	      if (! search_start)
		return;
	    }
	}
    }

  if (!search_start)
    {
      if (angle_brackets)
	search_start = CPP_OPTION (pfile, bracket_include);
      else
	search_start = pfile->buffer->search_from;

      if (!search_start)
	{
	  cpp_error (pfile, "No include path in which to find %s", fname);
	  return;
	}
    }

  inc = find_include_file (pfile, fname, search_start);
  if (inc)
    {
      if (angle_brackets)
	pfile->system_include_depth++;

      stack_include_file (pfile, inc);

      if (! DO_NOT_REREAD (inc))
	{
	  if (no_reinclude)
	    _cpp_never_reread (inc);

	  /* Handle -H option.  */
	  if (CPP_OPTION (pfile, print_include_names))
	    {
	      cpp_buffer *fp = pfile->buffer;
	      while ((fp = fp->prev) != NULL)
		putc ('.', stderr);
	      fprintf (stderr, " %s\n", inc->name);
	    }
	}

      return;
    }
      
  /* We will try making the RHS pfile->buffer->sysp after 3.0.  */
  print_dep = CPP_PRINT_DEPS(pfile) > (angle_brackets
				       || pfile->system_include_depth);
  if (CPP_OPTION (pfile, print_deps_missing_files) && print_dep)
    {
      if (!angle_brackets || IS_ABSOLUTE_PATHNAME (fname))
	deps_add_dep (pfile->deps, fname);
      else
	{
	  char *p;
	  struct search_path *ptr;
	  int len;

	  /* If requested as a system header, assume it belongs in
	     the first system header directory.  */
	  if (CPP_OPTION (pfile, bracket_include))
	    ptr = CPP_OPTION (pfile, bracket_include);
	  else
	    ptr = CPP_OPTION (pfile, quote_include);

	  /* FIXME: ptr can be null, no?  */
	  len = ptr->len;
	  p = (char *) alloca (len + header->val.str.len + 2);
	  if (len)
	    {
	      memcpy (p, ptr->name, len);
	      p[len++] = '/';
	    }
	  memcpy (p + len, fname, header->val.str.len + 1);
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
  else if (CPP_PRINT_DEPS (pfile) && ! print_dep)
    cpp_warning (pfile, "No include path in which to find %s", fname);
  else
    cpp_error_from_errno (pfile, fname);
}

/* Locate file F, and determine whether it is newer than PFILE. Return -1,
   if F cannot be located or dated, 1, if it is newer and 0 if older.  */
int
_cpp_compare_file_date (pfile, f)
     cpp_reader *pfile;
     const cpp_token *f;
{
  const char *fname = (const char *) f->val.str.text;
  struct search_path *search_start;
  struct include_file *inc;

  if (f->type == CPP_HEADER_NAME)
    search_start = CPP_OPTION (pfile, bracket_include);
  else if (CPP_OPTION (pfile, ignore_srcdir))
    search_start = pfile->buffer->search_from;

  inc = find_include_file (pfile, fname, search_start);
  
  if (!inc)
    return -1;
  if (inc->fd > 0)
    {
      close (inc->fd);
      inc->fd = -1;
    }
    
  return inc->st.st_mtime > CPP_BUFFER (pfile)->inc->st.st_mtime;
}


/* Push an input buffer and load it up with the contents of FNAME.
   If FNAME is "", read standard input.  */
int
_cpp_read_file (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  struct include_file *f = open_file (pfile, fname);

  if (f == NULL)
    {
      cpp_error_from_errno (pfile, fname);
      return 0;
    }

  stack_include_file (pfile, f);
  return 1;
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

  /* Record the inclusion-preventing macro and its definedness.  */
  if (pfile->mi_state == MI_OUTSIDE && inc->cmacro != NEVER_REREAD)
    {
      /* This could be NULL meaning no controlling macro.  */
      inc->cmacro = pfile->mi_cmacro;
      inc->defined = 1;
    }

  /* Invalidate control macros in the #including file.  */
  pfile->mi_state = MI_FAILED;

  inc->refcnt--;
  if (inc->refcnt == 0 && DO_NOT_REREAD (inc))
    purge_cache (inc);
}

/* Returns the first place in the include chain to start searching for
   "" includes.  This involves stripping away the basename of the
   current file, unless -I- was specified.  */
static struct search_path *
search_from (pfile, inc)
     cpp_reader *pfile;
     struct include_file *inc;
{
  cpp_buffer *buffer = pfile->buffer;
  unsigned int dlen;

  /* Ignore the current file's directory if -I- was given.  */
  if (CPP_OPTION (pfile, ignore_srcdir))
    return CPP_OPTION (pfile, quote_include);

  dlen = lbasename (inc->name) - inc->name;
  if (dlen)
    {
      /* We don't guarantee NAME is null-terminated.  This saves
	 allocating and freeing memory, and duplicating it when faking
	 buffers in cpp_push_buffer.  Drop a trailing '/'.  */
      buffer->dir.name = inc->name;
      if (dlen > 1)
	dlen--;
    }
  else
    {
      buffer->dir.name = ".";
      dlen = 1;
    }

  if (dlen > pfile->max_include_len)
    pfile->max_include_len = dlen;

  buffer->dir.len = dlen;
  buffer->dir.next = CPP_OPTION (pfile, quote_include);
  buffer->dir.sysp = buffer->sysp;

  return &buffer->dir;
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

  /* Check the cache of directories, and mappings in their remap file.  */
  for (map_list_ptr = CPP_OPTION (pfile, map_list); map_list_ptr;
       map_list_ptr = map_list_ptr->map_list_next)
    if (! strcmp (map_list_ptr->map_list_name, dirname))
      return map_list_ptr->map_list_map;

  map_list_ptr = ((struct file_name_map_list *)
		  xmalloc (sizeof (struct file_name_map_list)));
  map_list_ptr->map_list_name = xstrdup (dirname);

  /* The end of the list ends in NULL.  */
  map_list_ptr->map_list_map = NULL;

  name = (char *) alloca (strlen (dirname) + strlen (FILE_NAME_MAP_FILE) + 2);
  strcpy (name, dirname);
  if (*dirname)
    strcat (name, "/");
  strcat (name, FILE_NAME_MAP_FILE);
  f = fopen (name, "r");

  /* Silently return NULL if we cannot open.  */
  if (f)
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
	  if (IS_ABSOLUTE_PATHNAME (to))
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
  
  /* Add this information to the cache.  */
  map_list_ptr->map_list_next = CPP_OPTION (pfile, map_list);
  CPP_OPTION (pfile, map_list) = map_list_ptr;

  return map_list_ptr->map_list_map;
}  

/* Remap NAME based on the file_name_map (if any) for LOC. */

static char *
remap_filename (pfile, name, loc)
     cpp_reader *pfile;
     char *name;
     struct search_path *loc;
{
  struct file_name_map *map;
  const char *from, *p;
  char *dir, *dname;

  /* Get a null-terminated path.  */
  dname = alloca (loc->len + 1);
  memcpy (dname, loc->name, loc->len);
  dname[loc->len] = '\0';

  if (! loc->name_map)
    {
      loc->name_map = read_name_map (pfile, dname);
      if (! loc->name_map)
	return name;
    }
  
  /* FIXME: this doesn't look right - NAME has been simplified.  */
  from = name + loc->len + 1;
  
  for (map = loc->name_map; map; map = map->map_next)
    if (!strcmp (map->map_from, from))
      return map->map_to;

  /* Try to find a mapping file for the particular directory we are
     looking in.  Thus #include <sys/types.h> will look up sys/types.h
     in /usr/include/header.gcc and look up types.h in
     /usr/include/sys/header.gcc.  */
  p = strrchr (name, '/');
  if (!p)
    return name;

  /* We know p != name as absolute paths don't call remap_filename.  */
  if (p == name)
    cpp_ice (pfile, "absolute file name in remap_filename");

  dir = (char *) alloca (p - name + 1);
  memcpy (dir, name, p - name);
  dir[p - name] = '\0';
  from = p + 1;
  
  for (map = read_name_map (pfile, dir); map; map = map->map_next)
    if (! strcmp (map->map_from, from))
      return map->map_to;

  return name;
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
