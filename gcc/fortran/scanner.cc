/* Character scanner.
   Copyright (C) 2000-2022 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Set of subroutines to (ultimately) return the next character to the
   various matching subroutines.  This file's job is to read files and
   build up lines that are parsed by the parser.  This means that we
   handle continuation lines and "include" lines.

   The first thing the scanner does is to load an entire file into
   memory.  We load the entire file into memory for a couple reasons.
   The first is that we want to be able to deal with nonseekable input
   (pipes, stdin) and there is a lot of backing up involved during
   parsing.

   The second is that we want to be able to print the locus of errors,
   and an error on line 999999 could conflict with something on line
   one.  Given nonseekable input, we've got to store the whole thing.

   One thing that helps are the column truncation limits that give us
   an upper bound on the size of individual lines.  We don't store the
   truncated stuff.

   From the scanner's viewpoint, the higher level subroutines ask for
   new characters and do a lot of jumping backwards.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gfortran.h"
#include "toplev.h"	/* For set_src_pwd.  */
#include "debug.h"
#include "options.h"
#include "diagnostic-core.h"  /* For fatal_error. */
#include "cpp.h"
#include "scanner.h"

/* List of include file search directories.  */
gfc_directorylist *include_dirs, *intrinsic_modules_dirs;

static gfc_file *file_head, *current_file;

static int continue_flag, end_flag, gcc_attribute_flag;
/* If !$omp/!$acc occurred in current comment line.  */
static int openmp_flag, openacc_flag;
static int continue_count, continue_line;
static locus openmp_locus;
static locus openacc_locus;
static locus gcc_attribute_locus;

gfc_source_form gfc_current_form;
static gfc_linebuf *line_head, *line_tail;
       
locus gfc_current_locus;
const char *gfc_source_file;
static FILE *gfc_src_file;
static gfc_char_t *gfc_src_preprocessor_lines[2];

static struct gfc_file_change
{
  const char *filename;
  gfc_linebuf *lb;
  int line;
} *file_changes;
static size_t file_changes_cur, file_changes_count;
static size_t file_changes_allocated;

static gfc_char_t *last_error_char;

/* Functions dealing with our wide characters (gfc_char_t) and
   sequences of such characters.  */

int
gfc_wide_fits_in_byte (gfc_char_t c)
{
  return (c <= UCHAR_MAX);
}

static inline int
wide_is_ascii (gfc_char_t c)
{
  return (gfc_wide_fits_in_byte (c) && ((unsigned char) c & ~0x7f) == 0);
}

int
gfc_wide_is_printable (gfc_char_t c)
{
  return (gfc_wide_fits_in_byte (c) && ISPRINT ((unsigned char) c));
}

gfc_char_t
gfc_wide_tolower (gfc_char_t c)
{
  return (wide_is_ascii (c) ? (gfc_char_t) TOLOWER((unsigned char) c) : c);
}

gfc_char_t
gfc_wide_toupper (gfc_char_t c)
{
  return (wide_is_ascii (c) ? (gfc_char_t) TOUPPER((unsigned char) c) : c);
}

int
gfc_wide_is_digit (gfc_char_t c)
{
  return (c >= '0' && c <= '9');
}

static inline int
wide_atoi (gfc_char_t *c)
{
#define MAX_DIGITS 20
  char buf[MAX_DIGITS+1];
  int i = 0;

  while (gfc_wide_is_digit(*c) && i < MAX_DIGITS)
    buf[i++] = *c++;
  buf[i] = '\0';
  return atoi (buf);
}

size_t
gfc_wide_strlen (const gfc_char_t *str)
{
  size_t i;

  for (i = 0; str[i]; i++)
    ;

  return i;
}

gfc_char_t *
gfc_wide_memset (gfc_char_t *b, gfc_char_t c, size_t len)
{
  size_t i;

  for (i = 0; i < len; i++)
    b[i] = c;

  return b;
}

static gfc_char_t *
wide_strcpy (gfc_char_t *dest, const gfc_char_t *src)
{
  gfc_char_t *d;

  for (d = dest; (*d = *src) != '\0'; ++src, ++d)
    ;

  return dest;
}

static gfc_char_t *
wide_strchr (const gfc_char_t *s, gfc_char_t c)
{
  do {
    if (*s == c)
      {
        return CONST_CAST(gfc_char_t *, s);
      }
  } while (*s++);
  return 0;
}

char *
gfc_widechar_to_char (const gfc_char_t *s, int length)
{
  size_t len, i;
  char *res;

  if (s == NULL)
    return NULL;

  /* Passing a negative length is used to indicate that length should be
     calculated using gfc_wide_strlen().  */
  len = (length >= 0 ? (size_t) length : gfc_wide_strlen (s));
  res = XNEWVEC (char, len + 1);

  for (i = 0; i < len; i++)
    {
      gcc_assert (gfc_wide_fits_in_byte (s[i]));
      res[i] = (unsigned char) s[i];
    }

  res[len] = '\0';
  return res;
}

gfc_char_t *
gfc_char_to_widechar (const char *s)
{
  size_t len, i;
  gfc_char_t *res;

  if (s == NULL)
    return NULL;

  len = strlen (s);
  res = gfc_get_wide_string (len + 1);

  for (i = 0; i < len; i++)
    res[i] = (unsigned char) s[i];

  res[len] = '\0';
  return res;
}

static int
wide_strncmp (const gfc_char_t *s1, const char *s2, size_t n)
{
  gfc_char_t c1, c2;

  while (n-- > 0)
    {
      c1 = *s1++;
      c2 = *s2++;
      if (c1 != c2)
	return (c1 > c2 ? 1 : -1);
      if (c1 == '\0')
	return 0;
    }
  return 0;
}

int
gfc_wide_strncasecmp (const gfc_char_t *s1, const char *s2, size_t n)
{
  gfc_char_t c1, c2;

  while (n-- > 0)
    {
      c1 = gfc_wide_tolower (*s1++);
      c2 = TOLOWER (*s2++);
      if (c1 != c2)
	return (c1 > c2 ? 1 : -1);
      if (c1 == '\0')
	return 0;
    }
  return 0;
}


/* Main scanner initialization.  */

void
gfc_scanner_init_1 (void)
{
  file_head = NULL;
  line_head = NULL;
  line_tail = NULL;

  continue_count = 0;
  continue_line = 0;

  end_flag = 0;
  last_error_char = NULL;
}


/* Main scanner destructor.  */

void
gfc_scanner_done_1 (void)
{
  gfc_linebuf *lb;
  gfc_file *f;

  while(line_head != NULL) 
    {
      lb = line_head->next;
      free (line_head);
      line_head = lb;
    }
     
  while(file_head != NULL) 
    {
      f = file_head->next;
      free (file_head->filename);
      free (file_head);
      file_head = f;    
    }
}

static bool
gfc_do_check_include_dir (const char *path, bool warn)
{
  struct stat st;
  if (stat (path, &st))
    {
      if (errno != ENOENT)
	gfc_warning_now (0, "Include directory %qs: %s",
			 path, xstrerror(errno));
      else if (warn)
	  gfc_warning_now (OPT_Wmissing_include_dirs,
			   "Nonexistent include directory %qs", path);
      return false;
    }
  else if (!S_ISDIR (st.st_mode))
    {
      gfc_fatal_error ("%qs is not a directory", path);
      return false;
    }
  return true;
}

/* In order that -W(no-)missing-include-dirs works, the diagnostic can only be
   run after processing the commandline.  */
static void
gfc_do_check_include_dirs (gfc_directorylist **list, bool do_warn)
{
  gfc_directorylist *prev, *q, *n;
  prev = NULL;
  n = *list;
  while (n)
    {
      q = n; n = n->next;
      if (gfc_do_check_include_dir (q->path, q->warn && do_warn))
	{
	  prev = q;
	  continue;
	}
      if (prev == NULL)
	*list = n;
      else
	prev->next = n;
      free (q->path);
      free (q);
    }
}

void
gfc_check_include_dirs (bool verbose_missing_dir_warn)
{
  /* This is a bit convoluted: If gfc_cpp_enabled () and
     verbose_missing_dir_warn, the warning is shown by libcpp. Otherwise,
     it is shown here, still conditional on OPT_Wmissing_include_dirs.  */
  bool warn = !gfc_cpp_enabled () || !verbose_missing_dir_warn;
  gfc_do_check_include_dirs (&include_dirs, warn);
  gfc_do_check_include_dirs (&intrinsic_modules_dirs, verbose_missing_dir_warn);
  if (gfc_option.module_dir && gfc_cpp_enabled ())
    gfc_do_check_include_dirs (&include_dirs, true);
}

/* Adds path to the list pointed to by list.  */

static void
add_path_to_list (gfc_directorylist **list, const char *path,
		  bool use_for_modules, bool head, bool warn, bool defer_warn)
{
  gfc_directorylist *dir;
  const char *p;
  char *q;
  size_t len;
  int i;
  
  p = path;
  while (*p == ' ' || *p == '\t')  /* someone might do "-I include" */
    if (*p++ == '\0')
      return;

  /* Strip trailing directory separators from the path, as this
     will confuse Windows systems.  */
  len = strlen (p);
  q = (char *) alloca (len + 1);
  memcpy (q, p, len + 1);
  i = len - 1;
  while (i >=0 && IS_DIR_SEPARATOR (q[i]))
    q[i--] = '\0';

  if (!defer_warn && !gfc_do_check_include_dir (q, warn))
    return;

  if (head || *list == NULL)
    {
      dir = XCNEW (gfc_directorylist);
      if (!head)
        *list = dir;
    }
  else
    {
      dir = *list;
      while (dir->next)
	dir = dir->next;

      dir->next = XCNEW (gfc_directorylist);
      dir = dir->next;
    }

  dir->next = head ? *list : NULL;
  if (head)
    *list = dir;
  dir->use_for_modules = use_for_modules;
  dir->warn = warn;
  dir->path = xstrdup (p);
}

/* defer_warn is set to true while parsing the commandline.  */

void
gfc_add_include_path (const char *path, bool use_for_modules, bool file_dir,
		      bool warn, bool defer_warn)
{
  add_path_to_list (&include_dirs, path, use_for_modules, file_dir, warn,
		    defer_warn);

  /* For '#include "..."' these directories are automatically searched.  */
  if (!file_dir)
    gfc_cpp_add_include_path (xstrdup(path), true);
}


void
gfc_add_intrinsic_modules_path (const char *path)
{
  add_path_to_list (&intrinsic_modules_dirs, path, true, false, false, false);
}


/* Release resources allocated for options.  */

void
gfc_release_include_path (void)
{
  gfc_directorylist *p;

  while (include_dirs != NULL)
    {
      p = include_dirs;
      include_dirs = include_dirs->next;
      free (p->path);
      free (p);
    }

  while (intrinsic_modules_dirs != NULL)
    {
      p = intrinsic_modules_dirs;
      intrinsic_modules_dirs = intrinsic_modules_dirs->next;
      free (p->path);
      free (p);
    }

  free (gfc_option.module_dir);
}


static FILE *
open_included_file (const char *name, gfc_directorylist *list,
		    bool module, bool system)
{
  char *fullname;
  gfc_directorylist *p;
  FILE *f;

  for (p = list; p; p = p->next)
    {
      if (module && !p->use_for_modules)
	continue;

      fullname = (char *) alloca(strlen (p->path) + strlen (name) + 2);
      strcpy (fullname, p->path);
      strcat (fullname, "/");
      strcat (fullname, name);

      f = gfc_open_file (fullname);
      if (f != NULL)
	{
	  if (gfc_cpp_makedep ())
	    gfc_cpp_add_dep (fullname, system);

	  return f;
	}
    }

  return NULL;
}


/* Opens file for reading, searching through the include directories
   given if necessary.  If the include_cwd argument is true, we try
   to open the file in the current directory first.  */

FILE *
gfc_open_included_file (const char *name, bool include_cwd, bool module)
{
  FILE *f = NULL;

  if (IS_ABSOLUTE_PATH (name) || include_cwd)
    {
      f = gfc_open_file (name);
      if (f && gfc_cpp_makedep ())
	gfc_cpp_add_dep (name, false);
    }

  if (!f)
    f = open_included_file (name, include_dirs, module, false);

  return f;
}


/* Test to see if we're at the end of the main source file.  */

int
gfc_at_end (void)
{
  return end_flag;
}


/* Test to see if we're at the end of the current file.  */

int
gfc_at_eof (void)
{
  if (gfc_at_end ())
    return 1;

  if (line_head == NULL)
    return 1;			/* Null file */

  if (gfc_current_locus.lb == NULL)
    return 1;

  return 0;
}


/* Test to see if we're at the beginning of a new line.  */

int
gfc_at_bol (void)
{
  if (gfc_at_eof ())
    return 1;

  return (gfc_current_locus.nextc == gfc_current_locus.lb->line);
}


/* Test to see if we're at the end of a line.  */

int
gfc_at_eol (void)
{
  if (gfc_at_eof ())
    return 1;

  return (*gfc_current_locus.nextc == '\0');
}

static void
add_file_change (const char *filename, int line)
{
  if (file_changes_count == file_changes_allocated)
    {
      if (file_changes_allocated)
	file_changes_allocated *= 2;
      else
	file_changes_allocated = 16;
      file_changes = XRESIZEVEC (struct gfc_file_change, file_changes,
				 file_changes_allocated);
    }
  file_changes[file_changes_count].filename = filename;
  file_changes[file_changes_count].lb = NULL;
  file_changes[file_changes_count++].line = line;
}

static void
report_file_change (gfc_linebuf *lb)
{
  size_t c = file_changes_cur;
  while (c < file_changes_count
	 && file_changes[c].lb == lb)
    {
      if (file_changes[c].filename)
	(*debug_hooks->start_source_file) (file_changes[c].line,
					   file_changes[c].filename);
      else
	(*debug_hooks->end_source_file) (file_changes[c].line);
      ++c;
    }
  file_changes_cur = c;
}

void
gfc_start_source_files (void)
{
  /* If the debugger wants the name of the main source file,
     we give it.  */
  if (debug_hooks->start_end_main_source_file)
    (*debug_hooks->start_source_file) (0, gfc_source_file);

  file_changes_cur = 0;
  report_file_change (gfc_current_locus.lb);
}

void
gfc_end_source_files (void)
{
  report_file_change (NULL);

  if (debug_hooks->start_end_main_source_file)
    (*debug_hooks->end_source_file) (0);
}

/* Advance the current line pointer to the next line.  */

void
gfc_advance_line (void)
{
  if (gfc_at_end ())
    return;

  if (gfc_current_locus.lb == NULL) 
    {
      end_flag = 1;
      return;
    } 

  if (gfc_current_locus.lb->next
      && !gfc_current_locus.lb->next->dbg_emitted)
    {
      report_file_change (gfc_current_locus.lb->next);
      gfc_current_locus.lb->next->dbg_emitted = true;
    }

  gfc_current_locus.lb = gfc_current_locus.lb->next;

  if (gfc_current_locus.lb != NULL)	 
    gfc_current_locus.nextc = gfc_current_locus.lb->line;
  else 
    {
      gfc_current_locus.nextc = NULL;
      end_flag = 1;
    }       
}


/* Get the next character from the input, advancing gfc_current_file's
   locus.  When we hit the end of the line or the end of the file, we
   start returning a '\n' in order to complete the current statement.
   No Fortran line conventions are implemented here.

   Requiring explicit advances to the next line prevents the parse
   pointer from being on the wrong line if the current statement ends
   prematurely.  */

static gfc_char_t
next_char (void)
{
  gfc_char_t c;
  
  if (gfc_current_locus.nextc == NULL)
    return '\n';

  c = *gfc_current_locus.nextc++;
  if (c == '\0')
    {
      gfc_current_locus.nextc--; /* Remain on this line.  */
      c = '\n';
    }

  return c;
}


/* Skip a comment.  When we come here the parse pointer is positioned
   immediately after the comment character.  If we ever implement
   compiler directives within comments, here is where we parse the
   directive.  */

static void
skip_comment_line (void)
{
  gfc_char_t c;

  do
    {
      c = next_char ();
    }
  while (c != '\n');

  gfc_advance_line ();
}


int
gfc_define_undef_line (void)
{
  char *tmp;

  /* All lines beginning with '#' are either #define or #undef.  */
  if (debug_info_level != DINFO_LEVEL_VERBOSE || gfc_peek_ascii_char () != '#')
    return 0;

  if (wide_strncmp (gfc_current_locus.nextc, "#define ", 8) == 0)
    {
      tmp = gfc_widechar_to_char (&gfc_current_locus.nextc[8], -1);
      (*debug_hooks->define) (gfc_linebuf_linenum (gfc_current_locus.lb),
			      tmp);
      free (tmp);
    }

  if (wide_strncmp (gfc_current_locus.nextc, "#undef ", 7) == 0)
    {
      tmp = gfc_widechar_to_char (&gfc_current_locus.nextc[7], -1);
      (*debug_hooks->undef) (gfc_linebuf_linenum (gfc_current_locus.lb),
			     tmp);
      free (tmp);
    }

  /* Skip the rest of the line.  */
  skip_comment_line ();

  return 1;
}


/* Return true if GCC$ was matched.  */
static bool
skip_gcc_attribute (locus start)
{
  bool r = false;
  char c;
  locus old_loc = gfc_current_locus;

  if ((c = next_char ()) == 'g' || c == 'G')
    if ((c = next_char ()) == 'c' || c == 'C')
      if ((c = next_char ()) == 'c' || c == 'C')
	if ((c = next_char ()) == '$')
	  r = true;

  if (r == false)
    gfc_current_locus = old_loc;
  else
   {
      gcc_attribute_flag = 1;
      gcc_attribute_locus = old_loc;
      gfc_current_locus = start;
   }

  return r;
}

/* Return true if CC was matched.  */
static bool
skip_free_oacc_sentinel (locus start, locus old_loc)
{
  bool r = false;
  char c;

  if ((c = next_char ()) == 'c' || c == 'C')
    if ((c = next_char ()) == 'c' || c == 'C')
      r = true;

  if (r)
   {
      if ((c = next_char ()) == ' ' || c == '\t'
	  || continue_flag)
	{
	  while (gfc_is_whitespace (c))
	    c = next_char ();
	  if (c != '\n' && c != '!')
	    {
	      openacc_flag = 1;
	      openacc_locus = old_loc;
	      gfc_current_locus = start;
	    }
	  else 
	    r = false;
	}
      else
	{
	  gfc_warning_now (0, "!$ACC at %C starts a commented "
			   "line as it neither is followed "
			   "by a space nor is a "
			   "continuation line");
	  r = false;
	}
   }

  return r;
}

/* Return true if MP was matched.  */
static bool
skip_free_omp_sentinel (locus start, locus old_loc)
{
  bool r = false;
  char c;

  if ((c = next_char ()) == 'm' || c == 'M')
    if ((c = next_char ()) == 'p' || c == 'P')
      r = true;

  if (r)
   {
      if ((c = next_char ()) == ' ' || c == '\t'
	  || continue_flag)
	{
	  while (gfc_is_whitespace (c))
	    c = next_char ();
	  if (c != '\n' && c != '!')
	    {
	      openmp_flag = 1;
	      openmp_locus = old_loc;
	      gfc_current_locus = start;
	    }
	  else 
	    r = false;
	}
      else
	{
	  gfc_warning_now (0, "!$OMP at %C starts a commented "
			   "line as it neither is followed "
			   "by a space nor is a "
			   "continuation line");
	  r = false;
	}
   }

  return r;
}

/* Comment lines are null lines, lines containing only blanks or lines
   on which the first nonblank line is a '!'.
   Return true if !$ openmp or openacc conditional compilation sentinel was
   seen.  */

static bool
skip_free_comments (void)
{
  locus start;
  gfc_char_t c;
  int at_bol;

  for (;;)
    {
      at_bol = gfc_at_bol ();
      start = gfc_current_locus;
      if (gfc_at_eof ())
	break;

      do
	c = next_char ();
      while (gfc_is_whitespace (c));

      if (c == '\n')
	{
	  gfc_advance_line ();
	  continue;
	}

      if (c == '!')
	{
	  /* Keep the !GCC$ line.  */
	  if (at_bol && skip_gcc_attribute (start))
	    return false;

	  /* If -fopenmp/-fopenacc, we need to handle here 2 things:
	     1) don't treat !$omp/!$acc as comments, but directives
	     2) handle OpenMP/OpenACC conditional compilation, where
		!$ should be treated as 2 spaces (for initial lines
		only if followed by space).  */
	  if (at_bol)
	  {
	    if ((flag_openmp || flag_openmp_simd)
		&& flag_openacc)
	      {
		locus old_loc = gfc_current_locus;
		if (next_char () == '$')
		  {
		    c = next_char ();
		    if (c == 'o' || c == 'O')
		      {
			if (skip_free_omp_sentinel (start, old_loc))
			  return false;
			gfc_current_locus = old_loc;
			next_char ();
			c = next_char ();
		      }
		    else if (c == 'a' || c == 'A')
		      {
			if (skip_free_oacc_sentinel (start, old_loc))
			  return false;
			gfc_current_locus = old_loc;
			next_char ();
			c = next_char ();
		      }
		    if (continue_flag || c == ' ' || c == '\t')
		      {
			gfc_current_locus = old_loc;
			next_char ();
			openmp_flag = openacc_flag = 0;
			return true;
		      }
		  }
		gfc_current_locus = old_loc;
	      }
	    else if ((flag_openmp || flag_openmp_simd)
		     && !flag_openacc)
	      {
		locus old_loc = gfc_current_locus;
		if (next_char () == '$')
		  {
		    c = next_char ();
		    if (c == 'o' || c == 'O')
		      {
			if (skip_free_omp_sentinel (start, old_loc))
			  return false;
			gfc_current_locus = old_loc;
			next_char ();
			c = next_char ();
		      }
		    if (continue_flag || c == ' ' || c == '\t')
		      {
			gfc_current_locus = old_loc;
			next_char ();
			openmp_flag = 0;
			return true;
		      }
		  }
		gfc_current_locus = old_loc;
	      }
	    else if (flag_openacc
		     && !(flag_openmp || flag_openmp_simd))
	      {
		locus old_loc = gfc_current_locus;
		if (next_char () == '$')
		  {
		    c = next_char ();
		    if (c == 'a' || c == 'A')
		      {
			if (skip_free_oacc_sentinel (start, old_loc))
			  return false;
			gfc_current_locus = old_loc;
			next_char();
			c = next_char();
		      }
		  }
		gfc_current_locus = old_loc;
	      }
	  }
	  skip_comment_line ();
	  continue;
	}

      break;
    }

  if (openmp_flag && at_bol)
    openmp_flag = 0;

  if (openacc_flag && at_bol)
    openacc_flag = 0;

  gcc_attribute_flag = 0;
  gfc_current_locus = start;
  return false;
}

/* Return true if MP was matched in fixed form.  */
static bool
skip_fixed_omp_sentinel (locus *start)
{
  gfc_char_t c;
  if ((c = next_char ()) != 'm' && c != 'M')
    return false;
  if ((c = next_char ()) == 'p' || c == 'P')
    {
      c = next_char ();
      if (c != '\n'
	  && (continue_flag
	      || c == ' ' || c == '\t' || c == '0'))
	{
	  if (c == ' ' || c == '\t' || c == '0')
	    openacc_flag = 0;
	  do
	    c = next_char ();
	  while (gfc_is_whitespace (c));
	  if (c != '\n' && c != '!')
	    {
	      /* Canonicalize to *$omp.  */
	      *start->nextc = '*';
	      openmp_flag = 1;
	      gfc_current_locus = *start;
	      return true;
	    }
	}
    }
  else if (UNLIKELY (c == 'x' || c == 'X'))
    gfc_warning_now (OPT_Wsurprising,
		     "Ignoring '!$omx' vendor-extension sentinel at %C");
  return false;
}

/* Return true if CC was matched in fixed form.  */
static bool
skip_fixed_oacc_sentinel (locus *start)
{
  gfc_char_t c;
  if (((c = next_char ()) == 'c' || c == 'C')
      && ((c = next_char ()) == 'c' || c == 'C'))
    {
      c = next_char ();
      if (c != '\n'
	  && (continue_flag
	      || c == ' ' || c == '\t' || c == '0'))
	{
	  if (c == ' ' || c == '\t' || c == '0')
	    openmp_flag = 0;
	  do
	    c = next_char ();
	  while (gfc_is_whitespace (c));
	  if (c != '\n' && c != '!')
	    {
	      /* Canonicalize to *$acc.  */
	      *start->nextc = '*';
	      openacc_flag = 1;
	      gfc_current_locus = *start;
	      return true;
	    }
	}
    }
  return false;
}

/* Skip comment lines in fixed source mode.  We have the same rules as
   in skip_free_comment(), except that we can have a 'c', 'C' or '*'
   in column 1, and a '!' cannot be in column 6.  Also, we deal with
   lines with 'd' or 'D' in column 1, if the user requested this.  */

static void
skip_fixed_comments (void)
{
  locus start;
  int col;
  gfc_char_t c;

  if (! gfc_at_bol ())
    {
      start = gfc_current_locus;
      if (! gfc_at_eof ())
	{
	  do
	    c = next_char ();
	  while (gfc_is_whitespace (c));

	  if (c == '\n')
	    gfc_advance_line ();
	  else if (c == '!')
	    skip_comment_line ();
	}

      if (! gfc_at_bol ())
	{
	  gfc_current_locus = start;
	  return;
	}
    }

  for (;;)
    {
      start = gfc_current_locus;
      if (gfc_at_eof ())
	break;

      c = next_char ();
      if (c == '\n')
	{
	  gfc_advance_line ();
	  continue;
	}

      if (c == '!' || c == 'c' || c == 'C' || c == '*')
	{
	  if (skip_gcc_attribute (start))
	    {
	      /* Canonicalize to *$omp.  */
	      *start.nextc = '*';
	      return;
	    }

	  if (gfc_current_locus.lb != NULL
	      && continue_line < gfc_linebuf_linenum (gfc_current_locus.lb))
	    continue_line = gfc_linebuf_linenum (gfc_current_locus.lb);

	  /* If -fopenmp/-fopenacc, we need to handle here 2 things:
	     1) don't treat !$omp/!$acc|c$omp/c$acc|*$omp / *$acc as comments, 
		but directives
	     2) handle OpenMP/OpenACC conditional compilation, where
		!$|c$|*$ should be treated as 2 spaces if the characters
		in columns 3 to 6 are valid fixed form label columns
		characters.  */
	  if ((flag_openmp || flag_openmp_simd) && !flag_openacc)
	    {
	      if (next_char () == '$')
		{
		  c = next_char ();
		  if (c == 'o' || c == 'O')
		    {
		      if (skip_fixed_omp_sentinel (&start))
			return;
		    }
		  else
		    goto check_for_digits;
		}
	      gfc_current_locus = start;
	    }
	  else if (flag_openacc && !(flag_openmp || flag_openmp_simd))
	    {
	      if (next_char () == '$')
		{
		  c = next_char ();
		  if (c == 'a' || c == 'A')
		    {
		      if (skip_fixed_oacc_sentinel (&start))
			return;
		    }
		}
	      gfc_current_locus = start;
	    }
	  else if (flag_openacc || flag_openmp || flag_openmp_simd)
	    {
	      if (next_char () == '$')
		{
		  c = next_char ();
		  if (c == 'a' || c == 'A')
		    {
		      if (skip_fixed_oacc_sentinel (&start))
			return;
		    }
		  else if (c == 'o' || c == 'O')
		    {
		      if (skip_fixed_omp_sentinel (&start))
			return;
		    }
		  else
		    goto check_for_digits;
		}
	      gfc_current_locus = start;
	    }

	  skip_comment_line ();
	  continue;

check_for_digits:
	  {
	    /* Required for OpenMP's conditional compilation sentinel. */
	    int digit_seen = 0;

	    for (col = 3; col < 6; col++, c = next_char ())
	      if (c == ' ')
		continue;
	      else if (c == '\t')
		{
		  col = 6;
		  break;
		}
	      else if (c < '0' || c > '9')
		break;
	      else
		digit_seen = 1;

	    if (col == 6 && c != '\n'
		&& ((continue_flag && !digit_seen)
		    || c == ' ' || c == '\t' || c == '0'))
	      {
		gfc_current_locus = start;
		start.nextc[0] = ' ';
		start.nextc[1] = ' ';
		continue;
	      }
	    }
	  skip_comment_line ();
	  continue;
	}

      if (gfc_option.flag_d_lines != -1 && (c == 'd' || c == 'D'))
	{
	  if (gfc_option.flag_d_lines == 0)
	    {
	      skip_comment_line ();
	      continue;
	    }
	  else
	    *start.nextc = c = ' ';
	}

      col = 1;

      while (gfc_is_whitespace (c))
	{
	  c = next_char ();
	  col++;
	}

      if (c == '\n')
	{
	  gfc_advance_line ();
	  continue;
	}

      if (col != 6 && c == '!')
	{
	  if (gfc_current_locus.lb != NULL
	      && continue_line < gfc_linebuf_linenum (gfc_current_locus.lb))
	    continue_line = gfc_linebuf_linenum (gfc_current_locus.lb);
	  skip_comment_line ();
	  continue;
	}

      break;
    }

  openmp_flag = 0;
  openacc_flag = 0;
  gcc_attribute_flag = 0;
  gfc_current_locus = start;
}


/* Skips the current line if it is a comment.  */

void
gfc_skip_comments (void)
{
  if (gfc_current_form == FORM_FREE)
    skip_free_comments ();
  else
    skip_fixed_comments ();
}


/* Get the next character from the input, taking continuation lines
   and end-of-line comments into account.  This implies that comment
   lines between continued lines must be eaten here.  For higher-level
   subroutines, this flattens continued lines into a single logical
   line.  The in_string flag denotes whether we're inside a character
   context or not.  */

gfc_char_t
gfc_next_char_literal (gfc_instring in_string)
{
  static locus omp_acc_err_loc = {};
  locus old_loc;
  int i, prev_openmp_flag, prev_openacc_flag;
  gfc_char_t c;

  continue_flag = 0;
  prev_openacc_flag = prev_openmp_flag = 0;

restart:
  c = next_char ();
  if (gfc_at_end ())
    {
      continue_count = 0;
      return c;
    }

  if (gfc_current_form == FORM_FREE)
    {
      bool openmp_cond_flag;

      if (!in_string && c == '!')
	{
	  if (gcc_attribute_flag
	      && memcmp (&gfc_current_locus, &gcc_attribute_locus,
		 sizeof (gfc_current_locus)) == 0)
	    goto done;

	  if (openmp_flag
	      && memcmp (&gfc_current_locus, &openmp_locus,
		 sizeof (gfc_current_locus)) == 0)
	    goto done;

	  if (openacc_flag
	      && memcmp (&gfc_current_locus, &openacc_locus,
	         sizeof (gfc_current_locus)) == 0)
	    goto done;

	  /* This line can't be continued */
	  do
	    {
	      c = next_char ();
	    }
	  while (c != '\n');

	  /* Avoid truncation warnings for comment ending lines.  */
	  gfc_current_locus.lb->truncated = 0;

	  goto done;
	}

      /* Check to see if the continuation line was truncated.  */
      if (warn_line_truncation && gfc_current_locus.lb != NULL
	  && gfc_current_locus.lb->truncated)
	{
	  int maxlen = flag_free_line_length;
	  gfc_char_t *current_nextc = gfc_current_locus.nextc;

	  gfc_current_locus.lb->truncated = 0;
	  gfc_current_locus.nextc =  gfc_current_locus.lb->line + maxlen;
	  gfc_warning_now (OPT_Wline_truncation,
			   "Line truncated at %L", &gfc_current_locus);
	  gfc_current_locus.nextc = current_nextc;
	}

      if (c != '&')
	goto done;

      /* If the next nonblank character is a ! or \n, we've got a
	 continuation line.  */
      old_loc = gfc_current_locus;

      c = next_char ();
      while (gfc_is_whitespace (c))
	c = next_char ();

      /* Character constants to be continued cannot have commentary
	 after the '&'. However, there are cases where we may think we
	 are still in a string and we are looking for a possible
	 doubled quote and we end up here. See PR64506.  */

      if (in_string && c != '\n')
	{
	  gfc_current_locus = old_loc;
	  c = '&';
	  goto done;
	}

      if (c != '!' && c != '\n')
	{
	  gfc_current_locus = old_loc;
	  c = '&';
	  goto done;
	}

      if (flag_openmp)
	prev_openmp_flag = openmp_flag;
      if (flag_openacc)
	prev_openacc_flag = openacc_flag;

      /* This can happen if the input file changed or via cpp's #line
	 without getting reset (e.g. via input_stmt). It also happens
	 when pre-including files via -fpre-include=.  */
      if (continue_count == 0
	  && gfc_current_locus.lb
	  && continue_line > gfc_linebuf_linenum (gfc_current_locus.lb) + 1)
	continue_line = gfc_linebuf_linenum (gfc_current_locus.lb) + 1;

      continue_flag = 1;
      if (c == '!')
	skip_comment_line ();
      else
	gfc_advance_line ();
      
      if (gfc_at_eof ())
	goto not_continuation;

      /* We've got a continuation line.  If we are on the very next line after
	 the last continuation, increment the continuation line count and
	 check whether the limit has been exceeded.  */
      if (gfc_linebuf_linenum (gfc_current_locus.lb) == continue_line + 1)
	{
	  if (++continue_count == gfc_option.max_continue_free)
	    {
	      if (gfc_notification_std (GFC_STD_GNU) || pedantic)
		gfc_warning (0, "Limit of %d continuations exceeded in "
			     "statement at %C", gfc_option.max_continue_free);
	    }
	}

      /* Now find where it continues. First eat any comment lines.  */
      openmp_cond_flag = skip_free_comments ();

      if (gfc_current_locus.lb != NULL
	  && continue_line < gfc_linebuf_linenum (gfc_current_locus.lb))
	continue_line = gfc_linebuf_linenum (gfc_current_locus.lb);

      if (flag_openmp)
	if (prev_openmp_flag != openmp_flag && !openacc_flag)
	  {
	    gfc_current_locus = old_loc;
	    openmp_flag = prev_openmp_flag;
	    c = '&';
	    goto done;
	  }

      if (flag_openacc)
	if (prev_openacc_flag != openacc_flag && !openmp_flag)
	  {
	    gfc_current_locus = old_loc;
	    openacc_flag = prev_openacc_flag;
	    c = '&';
	    goto done;
	  }

      /* Now that we have a non-comment line, probe ahead for the
	 first non-whitespace character.  If it is another '&', then
	 reading starts at the next character, otherwise we must back
	 up to where the whitespace started and resume from there.  */

      old_loc = gfc_current_locus;

      c = next_char ();
      while (gfc_is_whitespace (c))
	c = next_char ();

      if (openmp_flag && !openacc_flag)
	{
	  for (i = 0; i < 5; i++, c = next_char ())
	    {
	      gcc_assert (gfc_wide_tolower (c) == (unsigned char) "!$omp"[i]);
	      if (i == 4)
		old_loc = gfc_current_locus;
	    }
	  while (gfc_is_whitespace (c))
	    c = next_char ();
	}
      if (openacc_flag && !openmp_flag)
	{
	  for (i = 0; i < 5; i++, c = next_char ())
	    {
	      gcc_assert (gfc_wide_tolower (c) == (unsigned char) "!$acc"[i]);
	      if (i == 4)
		old_loc = gfc_current_locus;
	    }
	  while (gfc_is_whitespace (c))
	    c = next_char ();
	}

      /* In case we have an OpenMP directive continued by OpenACC
	 sentinel, or vice versa, we get both openmp_flag and
	 openacc_flag on.  */

      if (openacc_flag && openmp_flag)
	{
	  int is_openmp = 0;
	  for (i = 0; i < 5; i++, c = next_char ())
	    {
	      if (gfc_wide_tolower (c) != (unsigned char) "!$acc"[i])
		is_openmp = 1;
	    }
	  if (omp_acc_err_loc.nextc != gfc_current_locus.nextc
	      || omp_acc_err_loc.lb != gfc_current_locus.lb)
	    gfc_error_now (is_openmp
			   ? G_("Wrong OpenACC continuation at %C: "
				"expected !$ACC, got !$OMP")
			   : G_("Wrong OpenMP continuation at %C: "
				"expected !$OMP, got !$ACC"));
	  omp_acc_err_loc = gfc_current_locus;
	  goto not_continuation;
	}

      if (c != '&')
	{
	  if (in_string && gfc_current_locus.nextc)
	    {
	      gfc_current_locus.nextc--;
	      if (warn_ampersand && in_string == INSTRING_WARN)
		gfc_warning (OPT_Wampersand, 
			     "Missing %<&%> in continued character "
			     "constant at %C");
	    }
	  else if (!in_string && (c == '\'' || c == '"'))
	      goto done;
	  /* Both !$omp and !$ -fopenmp continuation lines have & on the
	     continuation line only optionally.  */
	  else if (openmp_flag || openacc_flag || openmp_cond_flag)
	    {
	      if (gfc_current_locus.nextc)
		  gfc_current_locus.nextc--;
	    }
	  else
	    {
	      c = ' ';
	      gfc_current_locus = old_loc;
	      goto done;
	    }
	}
    }
  else /* Fixed form.  */
    {
      /* Fixed form continuation.  */
      if (in_string != INSTRING_WARN && c == '!')
	{
	  /* Skip comment at end of line.  */
	  do
	    {
	      c = next_char ();
	    }
	  while (c != '\n');

	  /* Avoid truncation warnings for comment ending lines.  */
	  gfc_current_locus.lb->truncated = 0;
	}

      if (c != '\n')
	goto done;

      /* Check to see if the continuation line was truncated.  */
      if (warn_line_truncation && gfc_current_locus.lb != NULL
	  && gfc_current_locus.lb->truncated)
	{
	  gfc_current_locus.lb->truncated = 0;
	  gfc_warning_now (OPT_Wline_truncation,
			   "Line truncated at %L", &gfc_current_locus);
	}

      if (flag_openmp)
	prev_openmp_flag = openmp_flag;
      if (flag_openacc)
	prev_openacc_flag = openacc_flag;

      /* This can happen if the input file changed or via cpp's #line
	 without getting reset (e.g. via input_stmt). It also happens
	 when pre-including files via -fpre-include=.  */
      if (continue_count == 0
	  && gfc_current_locus.lb
	  && continue_line > gfc_linebuf_linenum (gfc_current_locus.lb) + 1)
	continue_line = gfc_linebuf_linenum (gfc_current_locus.lb) + 1;

      continue_flag = 1;
      old_loc = gfc_current_locus;

      gfc_advance_line ();
      skip_fixed_comments ();

      /* See if this line is a continuation line.  */
      if (flag_openmp && openmp_flag != prev_openmp_flag && !openacc_flag)
	{
	  openmp_flag = prev_openmp_flag;
	  goto not_continuation;
	}
      if (flag_openacc && openacc_flag != prev_openacc_flag && !openmp_flag)
	{
	  openacc_flag = prev_openacc_flag;
	  goto not_continuation;
	}

      /* In case we have an OpenMP directive continued by OpenACC
	 sentinel, or vice versa, we get both openmp_flag and
	 openacc_flag on.  */
      if (openacc_flag && openmp_flag)
	{
	  int is_openmp = 0;
	  for (i = 0; i < 5; i++)
	    {
	      c = next_char ();
	      if (gfc_wide_tolower (c) != (unsigned char) "*$acc"[i])
		is_openmp = 1;
	    }
	  if (omp_acc_err_loc.nextc != gfc_current_locus.nextc
	      || omp_acc_err_loc.lb != gfc_current_locus.lb)
	    gfc_error_now (is_openmp
			   ? G_("Wrong OpenACC continuation at %C: "
				"expected !$ACC, got !$OMP")
			   : G_("Wrong OpenMP continuation at %C: "
				"expected !$OMP, got !$ACC"));
	  omp_acc_err_loc = gfc_current_locus;
	  goto not_continuation;
	}
      else if (!openmp_flag && !openacc_flag)
	for (i = 0; i < 5; i++)
	  {
	    c = next_char ();
	    if (c != ' ')
	      goto not_continuation;
	  }
      else if (openmp_flag)
	for (i = 0; i < 5; i++)
	  {
	    c = next_char ();
	    if (gfc_wide_tolower (c) != (unsigned char) "*$omp"[i])
	      goto not_continuation;
	  }
      else if (openacc_flag)
	for (i = 0; i < 5; i++)
	  {
	    c = next_char ();
	    if (gfc_wide_tolower (c) != (unsigned char) "*$acc"[i])
	      goto not_continuation;
	  }

      c = next_char ();
      if (c == '0' || c == ' ' || c == '\n')
	goto not_continuation;

      /* We've got a continuation line.  If we are on the very next line after
	 the last continuation, increment the continuation line count and
	 check whether the limit has been exceeded.  */
      if (gfc_linebuf_linenum (gfc_current_locus.lb) == continue_line + 1)
	{
	  if (++continue_count == gfc_option.max_continue_fixed)
	    {
	      if (gfc_notification_std (GFC_STD_GNU) || pedantic)
		gfc_warning (0, "Limit of %d continuations exceeded in "
			     "statement at %C",
			     gfc_option.max_continue_fixed);
	    }
	}

      if (gfc_current_locus.lb != NULL
	  && continue_line < gfc_linebuf_linenum (gfc_current_locus.lb))
	continue_line = gfc_linebuf_linenum (gfc_current_locus.lb);
    }

  /* Ready to read first character of continuation line, which might
     be another continuation line!  */
  goto restart;

not_continuation:
  c = '\n';
  gfc_current_locus = old_loc;
  end_flag = 0;

done:
  if (c == '\n')
    continue_count = 0;
  continue_flag = 0;
  return c;
}


/* Get the next character of input, folded to lowercase.  In fixed
   form mode, we also ignore spaces.  When matcher subroutines are
   parsing character literals, they have to call
   gfc_next_char_literal().  */

gfc_char_t
gfc_next_char (void)
{
  gfc_char_t c;

  do
    {
      c = gfc_next_char_literal (NONSTRING);
    }
  while (gfc_current_form == FORM_FIXED && gfc_is_whitespace (c));

  return gfc_wide_tolower (c);
}

char
gfc_next_ascii_char (void)
{
  gfc_char_t c = gfc_next_char ();

  return (gfc_wide_fits_in_byte (c) ? (unsigned char) c
				    : (unsigned char) UCHAR_MAX);
}


gfc_char_t
gfc_peek_char (void)
{
  locus old_loc;
  gfc_char_t c;

  old_loc = gfc_current_locus;
  c = gfc_next_char ();
  gfc_current_locus = old_loc;

  return c;
}


char
gfc_peek_ascii_char (void)
{
  gfc_char_t c = gfc_peek_char ();

  return (gfc_wide_fits_in_byte (c) ? (unsigned char) c
				    : (unsigned char) UCHAR_MAX);
}


/* Recover from an error.  We try to get past the current statement
   and get lined up for the next.  The next statement follows a '\n'
   or a ';'.  We also assume that we are not within a character
   constant, and deal with finding a '\'' or '"'.  */

void
gfc_error_recovery (void)
{
  gfc_char_t c, delim;

  if (gfc_at_eof ())
    return;

  for (;;)
    {
      c = gfc_next_char ();
      if (c == '\n' || c == ';')
	break;

      if (c != '\'' && c != '"')
	{
	  if (gfc_at_eof ())
	    break;
	  continue;
	}
      delim = c;

      for (;;)
	{
	  c = next_char ();

	  if (c == delim)
	    break;
	  if (c == '\n')
	    return;
	  if (c == '\\')
	    {
	      c = next_char ();
	      if (c == '\n')
		return;
	    }
	}
      if (gfc_at_eof ())
	break;
    }
}


/* Read ahead until the next character to be read is not whitespace.  */

void
gfc_gobble_whitespace (void)
{
  static int linenum = 0;
  locus old_loc;
  gfc_char_t c;

  do
    {
      old_loc = gfc_current_locus;
      c = gfc_next_char_literal (NONSTRING);
      /* Issue a warning for nonconforming tabs.  We keep track of the line
	 number because the Fortran matchers will often back up and the same
	 line will be scanned multiple times.  */
      if (warn_tabs && c == '\t')
	{
	  int cur_linenum = LOCATION_LINE (gfc_current_locus.lb->location);
	  if (cur_linenum != linenum)
	    {
	      linenum = cur_linenum;
	      gfc_warning_now (OPT_Wtabs, "Nonconforming tab character at %C");
	    }
	}
    }
  while (gfc_is_whitespace (c));

  if (!ISPRINT(c) && c != '\n' && last_error_char != gfc_current_locus.nextc)
    {
      char buf[20];
      last_error_char = gfc_current_locus.nextc;
      snprintf (buf, 20, "%2.2X", c);
      gfc_error_now ("Invalid character 0x%s at %C", buf);
    }

  gfc_current_locus = old_loc;
}


/* Load a single line into pbuf.

   If pbuf points to a NULL pointer, it is allocated.
   We truncate lines that are too long, unless we're dealing with
   preprocessor lines or if the option -ffixed-line-length-none is set,
   in which case we reallocate the buffer to fit the entire line, if
   need be.
   In fixed mode, we expand a tab that occurs within the statement
   label region to expand to spaces that leave the next character in
   the source region.

   If first_char is not NULL, it's a pointer to a single char value holding
   the first character of the line, which has already been read by the
   caller.  This avoids the use of ungetc().

   load_line returns whether the line was truncated.

   NOTE: The error machinery isn't available at this point, so we can't
	 easily report line and column numbers consistent with other 
	 parts of gfortran.  */

static int
load_line (FILE *input, gfc_char_t **pbuf, int *pbuflen, const int *first_char)
{
  int c, maxlen, i, preprocessor_flag, buflen = *pbuflen;
  int quoted = ' ', comment_ix = -1;
  bool seen_comment = false;
  bool first_comment = true;
  bool trunc_flag = false;
  bool seen_printable = false;
  bool seen_ampersand = false;
  bool found_tab = false;
  bool warned_tabs = false;
  gfc_char_t *buffer;

  /* Determine the maximum allowed line length.  */
  if (gfc_current_form == FORM_FREE)
    maxlen = flag_free_line_length;
  else if (gfc_current_form == FORM_FIXED)
    maxlen = flag_fixed_line_length;
  else
    maxlen = 72;

  if (*pbuf == NULL)
    {
      /* Allocate the line buffer, storing its length into buflen.
	 Note that if maxlen==0, indicating that arbitrary-length lines
	 are allowed, the buffer will be reallocated if this length is
	 insufficient; since 132 characters is the length of a standard
	 free-form line, we use that as a starting guess.  */
      if (maxlen > 0)
	buflen = maxlen;
      else
	buflen = 132;

      *pbuf = gfc_get_wide_string (buflen + 1);
    }

  i = 0;
  buffer = *pbuf;

  if (first_char)
    c = *first_char;
  else
    c = getc (input);

  /* In order to not truncate preprocessor lines, we have to
     remember that this is one.  */
  preprocessor_flag = (c == '#');

  for (;;)
    {
      if (c == EOF)
	break;

      if (c == '\n')
	{
	  /* Check for illegal use of ampersand. See F95 Standard 3.3.1.3.  */
	  if (gfc_current_form == FORM_FREE 
	      && !seen_printable && seen_ampersand)
	    {
	      if (pedantic)
		gfc_error_now ("%<&%> not allowed by itself in line %d",
			       current_file->line);
	      else
		gfc_warning_now (0, "%<&%> not allowed by itself in line %d",
				 current_file->line);
	    }
	  break;
	}

      if (c == '\r' || c == '\0')
	goto next_char;			/* Gobble characters.  */

      if (c == '&')
	{
	  if (seen_ampersand)
	    {
	      seen_ampersand = false;
	      seen_printable = true;
	    }
	  else
	    seen_ampersand = true;
	}

      if ((c != '&' && c != '!' && c != ' ') || (c == '!' && !seen_ampersand))
	seen_printable = true;

      /* Is this a fixed-form comment?  */
      if (gfc_current_form == FORM_FIXED && i == 0
	  && (c == '*' || c == 'c' || c == 'C'
	      || (gfc_option.flag_d_lines != -1 && (c == 'd' || c == 'D'))))
	{
	  seen_comment = true;
	  comment_ix = i;
	}

      if (quoted == ' ')
	{
	  if (c == '\'' || c == '"')
	    quoted = c;
	}
      else if (c == quoted)
	quoted = ' ';

      /* Is this a free-form comment?  */
      if (c == '!' && quoted == ' ')
	{
	  if (seen_comment)
	    first_comment = false;
	  seen_comment = true;
	  comment_ix = i;
	}

      /* For truncation and tab warnings, set seen_comment to false if one has
	 either an OpenMP or OpenACC directive - or a !GCC$ attribute.  If
	 OpenMP is enabled, use '!$' as conditional compilation sentinel
	 and OpenMP directive ('!$omp').  */
      if (seen_comment && first_comment && flag_openmp && comment_ix + 1 == i
	  && c == '$')
	first_comment = seen_comment = false;
      if (seen_comment && first_comment && comment_ix + 4 == i)
	{
	  if (((*pbuf)[comment_ix+1] == 'g' || (*pbuf)[comment_ix+1] == 'G')
	      && ((*pbuf)[comment_ix+2] == 'c' || (*pbuf)[comment_ix+2] == 'C')
	      && ((*pbuf)[comment_ix+3] == 'c' || (*pbuf)[comment_ix+3] == 'C')
	      && c == '$')
	    first_comment = seen_comment = false;
	  if (flag_openacc
	      && (*pbuf)[comment_ix+1] == '$'
	      && ((*pbuf)[comment_ix+2] == 'a' || (*pbuf)[comment_ix+2] == 'A')
	      && ((*pbuf)[comment_ix+3] == 'c' || (*pbuf)[comment_ix+3] == 'C')
	      && (c == 'c' || c == 'C'))
	    first_comment = seen_comment = false;
	}

      /* Vendor extension: "<tab>1" marks a continuation line.  */
      if (found_tab)
	{
	  found_tab = false;
	  if (c >= '1' && c <= '9')
	    {
	      *(buffer-1) = c;
	      goto next_char;
	    }
	}

      if (gfc_current_form == FORM_FIXED && c == '\t' && i < 6)
	{
	  found_tab = true;

	  if (warn_tabs && seen_comment == 0 && !warned_tabs)
	    {
	      warned_tabs = true;
	      gfc_warning_now (OPT_Wtabs,
			       "Nonconforming tab character in column %d "
			       "of line %d", i + 1, current_file->line);
	    }

	  while (i < 6)
	    {
	      *buffer++ = ' ';
	      i++;
	    }

	  goto next_char;
	}

      *buffer++ = c;
      i++;

      if (maxlen == 0 || preprocessor_flag)
	{
	  if (i >= buflen)
	    {
	      /* Reallocate line buffer to double size to hold the
		overlong line.  */
	      buflen = buflen * 2;
	      *pbuf = XRESIZEVEC (gfc_char_t, *pbuf, (buflen + 1));
	      buffer = (*pbuf) + i;
	    }
	}
      else if (i >= maxlen)
	{
	  bool trunc_warn = true;

	  /* Enhancement, if the very next non-space character is an ampersand
	     or comment that we would otherwise warn about, don't mark as
	     truncated.  */

	  /* Truncate the rest of the line.  */
	  for (;;)
	    {
	      c = getc (input);
	      if (c == '\r' || c == ' ')
	        continue;

	      if (c == '\n' || c == EOF)
		break;

	      if (!trunc_warn && c != '!')
		trunc_warn = true;

	      if (trunc_warn && ((gfc_current_form == FORM_FIXED && c == '&')
		  || c == '!'))
		trunc_warn = false;

	      if (c == '!')
		seen_comment = 1;

	      if (trunc_warn && !seen_comment)
		trunc_flag = 1;
	    }

	  c = '\n';
	  continue;
	}

next_char:
      c = getc (input);
    }

  /* Pad lines to the selected line length in fixed form.  */
  if (gfc_current_form == FORM_FIXED
      && flag_fixed_line_length != 0
      && flag_pad_source
      && !preprocessor_flag
      && c != EOF)
    {
      while (i++ < maxlen)
	*buffer++ = ' ';
    }

  *buffer = '\0';
  *pbuflen = buflen;

  return trunc_flag;
}


/* Get a gfc_file structure, initialize it and add it to
   the file stack.  */

static gfc_file *
get_file (const char *name, enum lc_reason reason)
{
  gfc_file *f;

  f = XCNEW (gfc_file);

  f->filename = xstrdup (name);

  f->next = file_head;
  file_head = f;

  f->up = current_file;
  if (current_file != NULL)
    f->inclusion_line = current_file->line;

  linemap_add (line_table, reason, false, f->filename, 1);

  return f;
}


/* Deal with a line from the C preprocessor. The
   initial octothorp has already been seen.  */

static void
preprocessor_line (gfc_char_t *c)
{
  bool flag[5];
  int i, line;
  gfc_char_t *wide_filename;
  gfc_file *f;
  int escaped, unescape;
  char *filename;

  c++;
  while (*c == ' ' || *c == '\t')
    c++;

  if (*c < '0' || *c > '9')
    goto bad_cpp_line;

  line = wide_atoi (c);

  c = wide_strchr (c, ' ');
  if (c == NULL)
    {
      /* No file name given.  Set new line number.  */
      current_file->line = line;
      return;
    }

  /* Skip spaces.  */
  while (*c == ' ' || *c == '\t')
    c++;

  /* Skip quote.  */
  if (*c != '"')
    goto bad_cpp_line;
  ++c;

  wide_filename = c;

  /* Make filename end at quote.  */
  unescape = 0;
  escaped = false;
  while (*c && ! (!escaped && *c == '"'))
    {
      if (escaped)
	escaped = false;
      else if (*c == '\\')
	{
	  escaped = true;
	  unescape++;
	}
      ++c;
    }

  if (! *c)
    /* Preprocessor line has no closing quote.  */
    goto bad_cpp_line;

  *c++ = '\0';

  /* Undo effects of cpp_quote_string.  */
  if (unescape)
    {
      gfc_char_t *s = wide_filename;
      gfc_char_t *d = gfc_get_wide_string (c - wide_filename - unescape);

      wide_filename = d;
      while (*s)
	{
	  if (*s == '\\')
	    *d++ = *++s;
	  else
	    *d++ = *s;
	  s++;
	}
      *d = '\0';
    }

  /* Get flags.  */

  flag[1] = flag[2] = flag[3] = flag[4] = false;

  for (;;)
    {
      c = wide_strchr (c, ' ');
      if (c == NULL)
	break;

      c++;
      i = wide_atoi (c);

      if (i >= 1 && i <= 4)
	flag[i] = true;
    }

  /* Convert the filename in wide characters into a filename in narrow
     characters.  */
  filename = gfc_widechar_to_char (wide_filename, -1);

  /* Interpret flags.  */

  if (flag[1]) /* Starting new file.  */
    {
      f = get_file (filename, LC_RENAME);
      add_file_change (f->filename, f->inclusion_line);
      current_file = f;
    }

  if (flag[2]) /* Ending current file.  */
    {
      if (!current_file->up
	  || filename_cmp (current_file->up->filename, filename) != 0)
	{
	  linemap_line_start (line_table, current_file->line, 80);
	  /* ??? One could compute the exact column where the filename
	     starts and compute the exact location here.  */
	  gfc_warning_now_at (linemap_position_for_column (line_table, 1),
			      0, "file %qs left but not entered",
			      filename);
	  current_file->line++;
	  if (unescape)
	    free (wide_filename);
	  free (filename);
	  return;
	}

      add_file_change (NULL, line);
      current_file = current_file->up;
      linemap_add (line_table, LC_RENAME, false, current_file->filename,
		   current_file->line);
    }

  /* The name of the file can be a temporary file produced by
     cpp. Replace the name if it is different.  */

  if (filename_cmp (current_file->filename, filename) != 0)
    {
       /* FIXME: we leak the old filename because a pointer to it may be stored
          in the linemap.  Alternative could be using GC or updating linemap to
          point to the new name, but there is no API for that currently.  */
      current_file->filename = xstrdup (filename);

      /* We need to tell the linemap API that the filename changed.  Just
	 changing current_file is insufficient.  */
      linemap_add (line_table, LC_RENAME, false, current_file->filename, line);
    }

  /* Set new line number.  */
  current_file->line = line;
  if (unescape)
    free (wide_filename);
  free (filename);
  return;

 bad_cpp_line:
  linemap_line_start (line_table, current_file->line, 80);
  /* ??? One could compute the exact column where the directive
     starts and compute the exact location here.  */
  gfc_warning_now_at (linemap_position_for_column (line_table, 2), 0,
		      "Illegal preprocessor directive");
  current_file->line++;
}


static void load_file (const char *, const char *, bool);

/* include_line()-- Checks a line buffer to see if it is an include
   line.  If so, we call load_file() recursively to load the included
   file.  We never return a syntax error because a statement like
   "include = 5" is perfectly legal.  We return 0 if no include was
   processed, 1 if we matched an include or -1 if include was
   partially processed, but will need continuation lines.  */

static int
include_line (gfc_char_t *line)
{
  gfc_char_t quote, *c, *begin, *stop;
  char *filename;
  const char *include = "include";
  bool allow_continuation = flag_dec_include;
  int i;

  c = line;

  if (flag_openmp || flag_openmp_simd)
    {
      if (gfc_current_form == FORM_FREE)
	{
	  while (*c == ' ' || *c == '\t')
	    c++;
	  if (*c == '!' && c[1] == '$' && (c[2] == ' ' || c[2] == '\t'))
	    c += 3;
	}
      else
	{
	  if ((*c == '!' || *c == 'c' || *c == 'C' || *c == '*')
	      && c[1] == '$' && c[2] == ' ')
	    c += 3;
	}
    }

  if (gfc_current_form == FORM_FREE)
    {
      while (*c == ' ' || *c == '\t')
	c++;
      if (gfc_wide_strncasecmp (c, "include", 7))
	{
	  if (!allow_continuation)
	    return 0;
	  for (i = 0; i < 7; ++i)
	    {
	      gfc_char_t c1 = gfc_wide_tolower (*c);
	      if (c1 != (unsigned char) include[i])
		break;
	      c++;
	    }
	  if (i == 0 || *c != '&')
	    return 0;
	  c++;
	  while (*c == ' ' || *c == '\t')
	    c++;
	  if (*c == '\0' || *c == '!')
	    return -1;
	  return 0;
	}

      c += 7;
    }
  else
    {
      while (*c == ' ' || *c == '\t')
	c++;
      if (flag_dec_include && *c == '0' && c - line == 5)
	{
	  c++;
	  while (*c == ' ' || *c == '\t')
	    c++;
	}
      if (c - line < 6)
	allow_continuation = false;
      for (i = 0; i < 7; ++i)
	{
	  gfc_char_t c1 = gfc_wide_tolower (*c);
	  if (c1 != (unsigned char) include[i])
	    break;
	  c++;
	  while (*c == ' ' || *c == '\t')
	    c++;
	}
      if (!allow_continuation)
	{
	  if (i != 7)
	    return 0;
	}
      else if (i != 7)
	{
	  if (i == 0)
	    return 0;

	  /* At the end of line or comment this might be continued.  */
	  if (*c == '\0' || *c == '!')
	    return -1;

	  return 0;
	}
    }

  while (*c == ' ' || *c == '\t')
    c++;

  /* Find filename between quotes.  */

  quote = *c++;
  if (quote != '"' && quote != '\'')
    {
      if (allow_continuation)
	{
	  if (gfc_current_form == FORM_FREE)
	    {
	      if (quote == '&')
		{
		  while (*c == ' ' || *c == '\t')
		    c++;
		  if (*c == '\0' || *c == '!')
		    return -1;
		}
	    }
	  else if (quote == '\0' || quote == '!')
	    return -1;
	}
      return 0;
    }

  begin = c;

  bool cont = false;
  while (*c != quote && *c != '\0')
    {
      if (allow_continuation && gfc_current_form == FORM_FREE)
	{
	  if (*c == '&')
	    cont = true;
	  else if (*c != ' ' && *c != '\t')
	    cont = false;
	}
      c++;
    }

  if (*c == '\0')
    {
      if (allow_continuation
	  && (cont || gfc_current_form != FORM_FREE))
	return -1;
      return 0;
    }

  stop = c++;

  while (*c == ' ' || *c == '\t')
    c++;

  if (*c != '\0' && *c != '!')
    return 0;

  /* We have an include line at this point.  */

  *stop = '\0'; /* It's ok to trash the buffer, as this line won't be
		   read by anything else.  */

  filename = gfc_widechar_to_char (begin, -1);
  load_file (filename, NULL, false);
  free (filename);
  return 1;
}

/* Similarly, but try to parse an INCLUDE statement, using gfc_next_char etc.
   APIs.  Return 1 if recognized as valid INCLUDE statement and load_file has
   been called, 0 if it is not a valid INCLUDE statement and -1 if eof has
   been encountered while parsing it.  */
static int
include_stmt (gfc_linebuf *b)
{
  int ret = 0, i, length;
  const char *include = "include";
  gfc_char_t c, quote = 0;
  locus str_locus;
  char *filename;

  continue_flag = 0;
  end_flag = 0;
  gcc_attribute_flag = 0;
  openmp_flag = 0;
  openacc_flag = 0;
  continue_count = 0;
  continue_line = 0;
  gfc_current_locus.lb = b;
  gfc_current_locus.nextc = b->line;

  gfc_skip_comments ();
  gfc_gobble_whitespace ();

  for (i = 0; i < 7; i++)
    {
      c = gfc_next_char ();
      if (c != (unsigned char) include[i])
	{
	  if (gfc_current_form == FORM_FIXED
	      && i == 0
	      && c == '0'
	      && gfc_current_locus.nextc == b->line + 6)
	    {
	      gfc_gobble_whitespace ();
	      i--;
	      continue;
	    }
	  gcc_assert (i != 0);
	  if (c == '\n')
	    {
	      gfc_advance_line ();
	      gfc_skip_comments ();
	      if (gfc_at_eof ())
		ret = -1;
	    }
	  goto do_ret;
	}
    }
  gfc_gobble_whitespace ();

  c = gfc_next_char ();
  if (c == '\'' || c == '"')
    quote = c;
  else
    {
      if (c == '\n')
	{
	  gfc_advance_line ();
	  gfc_skip_comments ();
	  if (gfc_at_eof ())
	    ret = -1;
	}
      goto do_ret;
    }

  str_locus = gfc_current_locus;
  length = 0;
  do
    {
      c = gfc_next_char_literal (INSTRING_NOWARN);
      if (c == quote)
	break;
      if (c == '\n')
	{
	  gfc_advance_line ();
	  gfc_skip_comments ();
	  if (gfc_at_eof ())
	    ret = -1;
	  goto do_ret;
	}
      length++;
    }
  while (1);

  gfc_gobble_whitespace ();
  c = gfc_next_char ();
  if (c != '\n')
    goto do_ret;

  gfc_current_locus = str_locus;
  ret = 1;
  filename = XNEWVEC (char, length + 1);
  for (i = 0; i < length; i++)
    {
      c = gfc_next_char_literal (INSTRING_WARN);
      gcc_assert (gfc_wide_fits_in_byte (c));
      filename[i] = (unsigned char) c;
    }
  filename[length] = '\0';
  load_file (filename, NULL, false);
  free (filename);

do_ret:
  continue_flag = 0;
  end_flag = 0;
  gcc_attribute_flag = 0;
  openmp_flag = 0;
  openacc_flag = 0;
  continue_count = 0;
  continue_line = 0;
  memset (&gfc_current_locus, '\0', sizeof (locus));
  memset (&openmp_locus, '\0', sizeof (locus));
  memset (&openacc_locus, '\0', sizeof (locus));
  memset (&gcc_attribute_locus, '\0', sizeof (locus));
  return ret;
}



/* Load a file into memory by calling load_line until the file ends.  */

static void
load_file (const char *realfilename, const char *displayedname, bool initial)
{
  gfc_char_t *line;
  gfc_linebuf *b, *include_b = NULL;
  gfc_file *f;
  FILE *input;
  int len, line_len;
  bool first_line;
  struct stat st;
  int stat_result;
  const char *filename;
  /* If realfilename and displayedname are different and non-null then
     surely realfilename is the preprocessed form of
     displayedname.  */
  bool preprocessed_p = (realfilename && displayedname
			 && strcmp (realfilename, displayedname));

  filename = displayedname ? displayedname : realfilename;

  for (f = current_file; f; f = f->up)
    if (filename_cmp (filename, f->filename) == 0)
      fatal_error (linemap_line_start (line_table, current_file->line, 0),
		   "File %qs is being included recursively", filename);
  if (initial)
    {
      if (gfc_src_file)
	{
	  input = gfc_src_file;
	  gfc_src_file = NULL;
	}
      else
	input = gfc_open_file (realfilename);

      if (input == NULL)
	gfc_fatal_error ("Cannot open file %qs", filename);
    }
  else
    {
      input = gfc_open_included_file (realfilename, false, false);
      if (input == NULL)
	{
	  /* For -fpre-include file, current_file is NULL.  */
	  if (current_file)
	    fatal_error (linemap_line_start (line_table, current_file->line, 0),
			 "Cannot open included file %qs", filename);
	  else
	    gfc_fatal_error ("Cannot open pre-included file %qs", filename);
	}
      stat_result = stat (realfilename, &st);
      if (stat_result == 0 && !S_ISREG (st.st_mode))
	{
	  fclose (input);
	  if (current_file)
	    fatal_error (linemap_line_start (line_table, current_file->line, 0),
			 "Included file %qs is not a regular file", filename);
	  else
	    gfc_fatal_error ("Included file %qs is not a regular file", filename);
	}
    }

  /* Load the file.

     A "non-initial" file means a file that is being included.  In
     that case we are creating an LC_ENTER map.

     An "initial" file means a main file; one that is not included.
     That file has already got at least one (surely more) line map(s)
     created by gfc_init.  So the subsequent map created in that case
     must have LC_RENAME reason.

     This latter case is not true for a preprocessed file.  In that
     case, although the file is "initial", the line maps created by
     gfc_init was used during the preprocessing of the file.  Now that
     the preprocessing is over and we are being fed the result of that
     preprocessing, we need to create a brand new line map for the
     preprocessed file, so the reason is going to be LC_ENTER.  */

  f = get_file (filename, (initial && !preprocessed_p) ? LC_RENAME : LC_ENTER);
  if (!initial)
    add_file_change (f->filename, f->inclusion_line);
  current_file = f;
  current_file->line = 1;
  line = NULL;
  line_len = 0;
  first_line = true;

  if (initial && gfc_src_preprocessor_lines[0])
    {
      preprocessor_line (gfc_src_preprocessor_lines[0]);
      free (gfc_src_preprocessor_lines[0]);
      gfc_src_preprocessor_lines[0] = NULL;
      if (gfc_src_preprocessor_lines[1])
	{
	  preprocessor_line (gfc_src_preprocessor_lines[1]);
	  free (gfc_src_preprocessor_lines[1]);
	  gfc_src_preprocessor_lines[1] = NULL;
	}
    }

  for (;;)
    {
      int trunc = load_line (input, &line, &line_len, NULL);
      int inc_line;

      len = gfc_wide_strlen (line);
      if (feof (input) && len == 0)
	break;

      /* If this is the first line of the file, it can contain a byte
	 order mark (BOM), which we will ignore:
	   FF FE is UTF-16 little endian,
	   FE FF is UTF-16 big endian,
	   EF BB BF is UTF-8.  */
      if (first_line
	  && ((line_len >= 2 && line[0] == (unsigned char) '\xFF'
			     && line[1] == (unsigned char) '\xFE')
	      || (line_len >= 2 && line[0] == (unsigned char) '\xFE'
			        && line[1] == (unsigned char) '\xFF')
	      || (line_len >= 3 && line[0] == (unsigned char) '\xEF'
				&& line[1] == (unsigned char) '\xBB'
				&& line[2] == (unsigned char) '\xBF')))
	{
	  int n = line[1] == (unsigned char) '\xBB' ? 3 : 2;
	  gfc_char_t *new_char = gfc_get_wide_string (line_len);

	  wide_strcpy (new_char, &line[n]);
	  free (line);
	  line = new_char;
	  len -= n;
	}

      /* There are three things this line can be: a line of Fortran
	 source, an include line or a C preprocessor directive.  */

      if (line[0] == '#')
	{
	  /* When -g3 is specified, it's possible that we emit #define
	     and #undef lines, which we need to pass to the middle-end
	     so that it can emit correct debug info.  */
	  if (debug_info_level == DINFO_LEVEL_VERBOSE
	      && (wide_strncmp (line, "#define ", 8) == 0
		  || wide_strncmp (line, "#undef ", 7) == 0))
	    ;
	  else
	    {
	      preprocessor_line (line);
	      continue;
	    }
	}

      /* Preprocessed files have preprocessor lines added before the byte
	 order mark, so first_line is not about the first line of the file
	 but the first line that's not a preprocessor line.  */
      first_line = false;

      inc_line = include_line (line);
      if (inc_line > 0)
	{
	  current_file->line++;
	  continue;
	}

      /* Add line.  */

      b = XCNEWVAR (gfc_linebuf, gfc_linebuf_header_size
		    + (len + 1) * sizeof (gfc_char_t));


      b->location
	= linemap_line_start (line_table, current_file->line++, len);
      /* ??? We add the location for the maximum column possible here,
	 because otherwise if the next call creates a new line-map, it
	 will not reserve space for any offset.  */
      if (len > 0)
	linemap_position_for_column (line_table, len);

      b->file = current_file;
      b->truncated = trunc;
      wide_strcpy (b->line, line);

      if (line_head == NULL)
	line_head = b;
      else
	line_tail->next = b;

      line_tail = b;

      while (file_changes_cur < file_changes_count)
	file_changes[file_changes_cur++].lb = b;

      if (flag_dec_include)
	{
	  if (include_b && b != include_b)
	    {
	      int inc_line2 = include_stmt (include_b);
	      if (inc_line2 == 0)
		include_b = NULL;
	      else if (inc_line2 > 0)
		{
		  do
		    {
		      if (gfc_current_form == FORM_FIXED)
			{
			  for (gfc_char_t *p = include_b->line; *p; p++)
			    *p = ' ';
			}
		      else
			include_b->line[0] = '\0';
                      if (include_b == b)
			break;
		      include_b = include_b->next;
		    }
		  while (1);
		  include_b = NULL;
		}
	    }
	  if (inc_line == -1 && !include_b)
	    include_b = b;
	}
    }

  /* Release the line buffer allocated in load_line.  */
  free (line);

  fclose (input);

  if (!initial)
    add_file_change (NULL, current_file->inclusion_line + 1);
  current_file = current_file->up;
  linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
}


/* Open a new file and start scanning from that file. Returns true
   if everything went OK, false otherwise.  If form == FORM_UNKNOWN
   it tries to determine the source form from the filename, defaulting
   to free form.  */

void
gfc_new_file (void)
{
  if (flag_pre_include != NULL)
    load_file (flag_pre_include, NULL, false);

  if (gfc_cpp_enabled ())
    {
      gfc_cpp_preprocess (gfc_source_file);
      if (!gfc_cpp_preprocess_only ())
	load_file (gfc_cpp_temporary_file (), gfc_source_file, true);
    }
  else
    load_file (gfc_source_file, NULL, true);

  gfc_current_locus.lb = line_head;
  gfc_current_locus.nextc = (line_head == NULL) ? NULL : line_head->line;

#if 0 /* Debugging aid.  */
  for (; line_head; line_head = line_head->next)
    printf ("%s:%3d %s\n", LOCATION_FILE (line_head->location),
	    LOCATION_LINE (line_head->location), line_head->line);

  exit (SUCCESS_EXIT_CODE);
#endif
}

static char *
unescape_filename (const char *ptr)
{
  const char *p = ptr, *s;
  char *d, *ret;
  int escaped, unescape = 0;

  /* Make filename end at quote.  */
  escaped = false;
  while (*p && ! (! escaped && *p == '"'))
    {
      if (escaped)
	escaped = false;
      else if (*p == '\\')
	{
	  escaped = true;
	  unescape++;
	}
      ++p;
    }

  if (!*p || p[1])
    return NULL;

  /* Undo effects of cpp_quote_string.  */
  s = ptr;
  d = XCNEWVEC (char, p + 1 - ptr - unescape);
  ret = d;

  while (s != p)
    {
      if (*s == '\\')
	*d++ = *++s;
      else
	*d++ = *s;
      s++;
    }
  *d = '\0';
  return ret;
}

/* For preprocessed files, if the first tokens are of the form # NUM.
   handle the directives so we know the original file name.  */

const char *
gfc_read_orig_filename (const char *filename, const char **canon_source_file)
{
  int c, len;
  char *dirname, *tmp;

  gfc_src_file = gfc_open_file (filename);
  if (gfc_src_file == NULL)
    return NULL;

  c = getc (gfc_src_file);

  if (c != '#')
    return NULL;

  len = 0;
  load_line (gfc_src_file, &gfc_src_preprocessor_lines[0], &len, &c);

  if (wide_strncmp (gfc_src_preprocessor_lines[0], "# 1 \"", 5) != 0)
    return NULL;

  tmp = gfc_widechar_to_char (&gfc_src_preprocessor_lines[0][5], -1);
  filename = unescape_filename (tmp);
  free (tmp);
  if (filename == NULL)
    return NULL;

  c = getc (gfc_src_file);

  if (c != '#')
    return filename;

  len = 0;
  load_line (gfc_src_file, &gfc_src_preprocessor_lines[1], &len, &c);

  if (wide_strncmp (gfc_src_preprocessor_lines[1], "# 1 \"", 5) != 0)
    return filename;

  tmp = gfc_widechar_to_char (&gfc_src_preprocessor_lines[1][5], -1);
  dirname = unescape_filename (tmp);
  free (tmp);
  if (dirname == NULL)
    return filename;

  len = strlen (dirname);
  if (len < 3 || dirname[len - 1] != '/' || dirname[len - 2] != '/')
    {
      free (dirname);
      return filename;
    }
  dirname[len - 2] = '\0';
  set_src_pwd (dirname);

  if (! IS_ABSOLUTE_PATH (filename))
    {
      char *p = XCNEWVEC (char, len + strlen (filename));

      memcpy (p, dirname, len - 2);
      p[len - 2] = '/';
      strcpy (p + len - 1, filename);
      *canon_source_file = p;
    }

  free (dirname);
  return filename;
}
