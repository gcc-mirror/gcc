/* fix-header.c - Make C header file suitable for C++.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This program massages a system include file (such as stdio.h),
   into a form more conformant with ANSI/POSIX, and more suitable for C++:

   * extern "C" { ... } braces are added (inside #ifndef __cplusplus),
   if they seem to be needed.  These prevent C++ compilers from name
   mangling the functions inside the braces.

   * If an old-style incomplete function declaration is seen (without
   an argument list), and it is a "standard" function listed in
   the file sys-protos.h (and with a non-empty argument list), then
   the declaration is converted to a complete prototype by replacing
   the empty parameter list with the argument lust from sys-protos.h.

   * The program can be given a list of (names of) required standard
   functions (such as fclose for stdio.h).  If a reqquired function
   is not seen in the input, then a prototype for it will be
   written to the output.

   * If all of the non-comment code of the original file is protected
   against multiple inclusion:
	#ifndef FOO
	#define FOO
	<body of include file>
	#endif
   then extra matter added to the include file is placed inside the <body>.

   * If the input file is OK (nothing needs to be done);
   the output file is not written (nor removed if it exists).

   There are also some special actions that are done for certain
   well-known standard include files:

   * If argv[1] is "sys/stat.h", the Posix.1 macros
   S_ISBLK, S_ISCHR, S_ISDIR, S_ISFIFO, S_ISLNK, S_ISREG are added if
   they were missing, and the corresponding "traditional" S_IFxxx
   macros were defined.

   * If argv[1] is "errno.h", errno is declared if it was missing.

   * TODO:  The input file should be read complete into memory, because:
   a) it needs to be scanned twice anyway, and
   b) it would be nice to allow update in place.

   Usage:
	fix-header FOO.H INFILE.H OUTFILE.H REQUIRED_FUNCS <SCAN-FILE
   where:
   * FOO.H is the relative file name of the include file,
   as it would be #include'd by a C file.  (E.g. stdio.h)
   * INFILE.H is a full pathname for the input file (e.g. /usr/include/stdio.h)
   * OUTFILE.H is the full pathname for where to write the output file,
   if anything needs to be done.  (e.g. ./include/stdio.h)
   * SCAN-FILE is the output of the scan-decls program.
   * REQUIRED_FUNCS is a list of required function (e.g. fclose for stdio.h).

   Written by Per Bothner <bothner@cygnus.com>, July 1993. */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef O_RDONLY
#define O_RDONLY 0
#endif
#include "hconfig.h"
#include "obstack.h"
#include "scan.h"

extern sstring buf;

int verbose = 0;
int partial_count = 0;
#if 0
/* All uses of this are ifdefed out.  This is no longer needed, because
   cccp.c implicitly forces the standard include files to be treated as C.
   Adding an explicit extern "C" is undesireable as it breaks the SunOS 4.x
   sun4c/romvec.h file.  */
int missing_extern_C_count = 0;
#endif
int missing_errno = 0;

#include "xsys-protos.h"

char *inf_buffer;
char *inf_limit;
char *inf_ptr;

/* Certain standard files get extra treatment */

enum special_file
{
  no_special,
  errno_special,
  sys_stat_special
};

enum special_file special_file_handling = no_special;

/* The following are only used when handling sys/stat.h */
/* They are set if the corresponding macro has been seen. */
int seen_S_IFBLK = 0, seen_S_ISBLK  = 0;
int seen_S_IFCHR = 0, seen_S_ISCHR  = 0;
int seen_S_IFDIR = 0, seen_S_ISDIR  = 0;
int seen_S_IFIFO = 0, seen_S_ISFIFO = 0;
int seen_S_IFLNK = 0, seen_S_ISLNK  = 0;
int seen_S_IFREG = 0, seen_S_ISREG  = 0;

/* Wrapper around free, to avoid prototype clashes. */

void
xfree (ptr)
     char *ptr;
{
  free (ptr);
}

/* Avoid error if config defines abort as fancy_abort.
   It's not worth "really" implementing this because ordinary
   compiler users never run fix-header.  */

void
fancy_abort ()
{
  abort ();
}

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free xfree
struct obstack scan_file_obstack;

/* NOTE:  If you edit this, also edit gen-protos.c !! */
struct fn_decl *
lookup_std_proto (name)
     char *name;
{
  int i = hash (name) % HASH_SIZE;
  int i0 = i;
  for (;;)
    {
      struct fn_decl *fn;
      if (hash_tab[i] == 0)
	return NULL;
      fn = &std_protos[hash_tab[i]];
      if (strcmp (fn->fname, name) == 0)
	return fn;
      i = (i+1) % HASH_SIZE;
      if (i == i0)
	abort ();
    }
}

char *inc_filename;
int inc_filename_length;
char *progname = "fix-header";
FILE *outf;
sstring line;

int lbrac_line, rbrac_line;

char **required_functions;
int required_unseen_count;

void 
write_lbrac ()
{
  
#if 0
  if (missing_extern_C_count + required_unseen_count > 0)
    fprintf (outf, "#ifdef __cplusplus\nextern \"C\" {\n#endif\n");
#endif

  if (partial_count)
    {
      fprintf (outf, "#ifndef _PARAMS\n");
      fprintf (outf, "#if defined(__STDC__) || defined(__cplusplus)\n");
      fprintf (outf, "#define _PARAMS(ARGS) ARGS\n");
      fprintf (outf, "#else\n");
      fprintf (outf, "#define _PARAMS(ARGS) ()\n");
      fprintf (outf, "#endif\n#endif /* _PARAMS */\n");
    }
}

struct partial_proto
{
  struct partial_proto *next;
  char *fname;	/* name of function */
  char *rtype;	/* return type */
  struct fn_decl *fn;
  int line_seen;
};

struct partial_proto *partial_proto_list = NULL;

struct partial_proto required_dummy_proto;
#define REQUIRED(FN) ((FN)->partial == &required_dummy_proto)
#define SET_REQUIRED(FN) ((FN)->partial = &required_dummy_proto)
#define CLEAR_REQUIRED(FN) ((FN)->partial = 0)

void
recognized_macro (fname)
     char *fname;
{
  /* The original include file defines fname as a macro. */
  struct fn_decl *fn = lookup_std_proto (fname);

  /* Since fname is a macro, don't require a prototype for it. */
  if (fn && REQUIRED (fn))
    {
      CLEAR_REQUIRED (fn);
      required_unseen_count--;
    }

  switch (special_file_handling)
    {
    case errno_special:
      if (strcmp (fname, "errno") == 0) missing_errno = 0;
      break;
    case sys_stat_special:
      if (fname[0] == 'S' && fname[1] == '_')
	{
	  if (strcmp (fname, "S_IFBLK") == 0) seen_S_IFBLK++;
	  else if (strcmp (fname, "S_ISBLK") == 0) seen_S_ISBLK++;
	  else if (strcmp (fname, "S_IFCHR") == 0) seen_S_IFCHR++;
	  else if (strcmp (fname, "S_ISCHR") == 0) seen_S_ISCHR++;
	  else if (strcmp (fname, "S_IFDIR") == 0) seen_S_IFDIR++;
	  else if (strcmp (fname, "S_ISDIR") == 0) seen_S_ISDIR++;
	  else if (strcmp (fname, "S_IFIFO") == 0) seen_S_IFIFO++;
	  else if (strcmp (fname, "S_ISFIFO") == 0) seen_S_ISFIFO++;
	  else if (strcmp (fname, "S_IFLNK") == 0) seen_S_IFLNK++;
	  else if (strcmp (fname, "S_ISLNK") == 0) seen_S_ISLNK++;
	  else if (strcmp (fname, "S_IFREG") == 0) seen_S_IFREG++;
	  else if (strcmp (fname, "S_ISREG") == 0) seen_S_ISREG++;
	}
    }
}

void
recognized_extern (name, type)
     char *name;
     char *type;
{
  switch (special_file_handling)
    {
    case errno_special:
      if (strcmp (name, "errno") == 0) missing_errno = 0;
      break;
    }
}

/* Called by scan_decls if it saw a function definition for a function
   named FNAME, with return type RTYPE, and argument list ARGS,
   in source file FILE_SEEN on line LINE_SEEN.
   KIND is 'I' for an inline function;
   'F' if a normal function declaration preceded by 'extern "C"'
   (or nested inside 'extern "C"' braces); or
   'f' for other function declarations. */

void
recognized_function (fname, kind, rtype, args, file_seen, line_seen)
     char *fname;
     int kind; /* One of 'f' 'F' or 'I' */
     char *rtype;
     char *args;
     char *file_seen;
     int line_seen;
{
  struct partial_proto *partial;
  int i;
  struct fn_decl *fn;
#if 0
  if (kind == 'f')
    missing_extern_C_count++;
#endif

  fn = lookup_std_proto (fname);

  /* Remove the function from the list of required function. */
  if (fn && REQUIRED (fn))
    {
      CLEAR_REQUIRED (fn);
      required_unseen_count--;
    }

  /* If we have a full prototype, we're done. */
  if (args[0] != '\0')
    return;
      
  if (kind == 'I')  /* don't edit inline function */
    return;

  /* If the partial prototype was included from some other file,
     we don't need to patch it up (in this run). */
  i = strlen (file_seen);
  if (i < inc_filename_length
      || strcmp (inc_filename, file_seen + (i - inc_filename_length)) != 0)
    return;

  if (fn == NULL)
    return;
  if (fn->params[0] == '\0' || strcmp (fn->params, "void") == 0)
    return;

  /* We only have a partial function declaration,
     so remember that we have to add a complete prototype. */
  partial_count++;
  partial = (struct partial_proto*)
    obstack_alloc (&scan_file_obstack, sizeof (struct partial_proto));
  partial->fname = obstack_alloc (&scan_file_obstack, strlen (fname) + 1);
  strcpy (partial->fname, fname);
  partial->rtype = obstack_alloc (&scan_file_obstack, strlen (rtype) + 1);
  strcpy (partial->rtype, rtype);
  partial->line_seen = line_seen;
  partial->fn = fn;
  fn->partial = partial;
  partial->next = partial_proto_list;
  partial_proto_list = partial;
  if (verbose)
    {
      fprintf (stderr, "(%s: %s non-prototype function declaration.)\n",
	       inc_filename, fname);
    }
}

void
read_scan_file (scan_file)
     FILE *scan_file;
{
  obstack_init (&scan_file_obstack); 

  scan_decls (scan_file);

  if (required_unseen_count + partial_count + missing_errno
#if 0
      + missing_extern_C_count
#endif      
      == 0)
    {
      if (verbose)
	fprintf (stderr, "%s: OK, nothing needs to be done.\n", inc_filename);
      exit (0);
    }
  if (!verbose)
    fprintf (stderr, "%s: fixing %s\n", progname, inc_filename);
  else
    {
      if (required_unseen_count)
	fprintf (stderr, "%s: %d missing function declarations.\n",
		 inc_filename, required_unseen_count);
      if (partial_count)
	fprintf (stderr, "%s: %d non-prototype function declarations.\n",
		 inc_filename, partial_count);
#if 0
      if (missing_extern_C_count)
	fprintf (stderr,
		 "%s: %d declarations not protected by extern \"C\".\n",
		 inc_filename, missing_extern_C_count);
#endif
    }
}

void
write_rbrac ()
{
  struct fn_decl *fn;
  char **rptr;

  if (required_unseen_count)
    {
      fprintf (outf,
	"#if defined(__cplusplus) || defined(__USE_FIXED_PROTOTYPES__)\n");
#ifdef NO_IMPLICIT_EXTERN_C
      fprintf (outf, "#ifdef __cplusplus\nextern \"C\" {\n#endif\n");
#endif
    }

  /* Now we print out prototypes for those functions that we haven't seen. */
  for (rptr = required_functions; *rptr; rptr++)
    {
      int macro_protect = 0;

      fn = lookup_std_proto (*rptr);
      if (fn == NULL || !REQUIRED (fn))
	continue;

      /* In the case of memmove, protect in case the application
	 defines it as a macro before including the header.  */
      if (!strcmp (fn->fname, "memmove")
	  || !strcmp (fn->fname, "vprintf")
	  || !strcmp (fn->fname, "vfprintf")
	  || !strcmp (fn->fname, "vsprintf")
	  || !strcmp (fn->fname, "rewinddir"))
	macro_protect = 1;

      if (macro_protect)
	fprintf (outf, "#ifndef %s\n", fn->fname);
      fprintf (outf, "extern %s %s (%s);\n",
	       fn->rtype, fn->fname, fn->params);
      if (macro_protect)
	fprintf (outf, "#endif\n");
    }
  if (required_unseen_count)
    {
#ifdef NO_IMPLICIT_EXTERN_C
      fprintf (outf, "#ifdef __cplusplus\n}\n#endif\n");
#endif
      fprintf (outf,
	"#endif /* defined(__cplusplus) || defined(__USE_FIXED_PROTOTYPES__*/\n");
    }

  switch (special_file_handling)
    {
    case errno_special:
      if (missing_errno)
	fprintf (outf, "extern int errno;\n");
      break;
    case sys_stat_special:
      if (!seen_S_ISBLK && seen_S_IFBLK)
	fprintf (outf,
		 "#define S_ISBLK(mode) (((mode) & S_IFMT) == S_IFBLK)\n");
      if (!seen_S_ISCHR && seen_S_IFCHR)
	fprintf (outf,
		 "#define S_ISCHR(mode) (((mode) & S_IFMT) == S_IFCHR)\n");
      if (!seen_S_ISDIR && seen_S_IFDIR)
	fprintf (outf,
		 "#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)\n");
      if (!seen_S_ISFIFO && seen_S_IFIFO)
	fprintf (outf,
		 "#define S_ISFIFO(mode) (((mode) & S_IFMT) == S_IFIFO)\n");
      if (!seen_S_ISLNK && seen_S_IFLNK)
	fprintf (outf,
		 "#define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)\n");
      if (!seen_S_ISREG && seen_S_IFREG)
	fprintf (outf,
		 "#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)\n");
      break;
    }


#if 0
  if (missing_extern_C_count + required_unseen_count > 0)
    fprintf (outf, "#ifdef __cplusplus\n}\n#endif\n");
#endif
}

char *
xstrdup (str)
     char *str;
{
  char *copy = (char *) xmalloc (strlen (str) + 1);
  strcpy (copy, str);
  return copy;
}

/* Returns 1 iff the file is properly protected from multiple inclusion:
   #ifndef PROTECT_NAME
   #define PROTECT_NAME
   #endif

 */

#define INF_GET() (inf_ptr < inf_limit ? *(unsigned char*)inf_ptr++ : EOF)
#define INF_UNGET(c) ((c)!=EOF && inf_ptr--)

int
inf_skip_spaces (c)
     int c;
{
  for (;;)
    {
      if (c == ' ' || c == '\t')
	c = INF_GET ();
      else if (c == '/')
	{
	  c = INF_GET ();
	  if (c != '*')
	    {
	      INF_UNGET (c);
	      return '/';
	    }
	  c = INF_GET ();
	  for (;;)
	    {
	      if (c == EOF)
		return EOF;
	      else if (c != '*')
		{
		  if (c == '\n')
		    source_lineno++, lineno++;
		  c = INF_GET ();
		}
	      else if ((c = INF_GET ()) == '/')
		return INF_GET ();
	    }
	}
      else
	break;
    }
  return c;
}

/* Read into STR from inf_buffer upto DELIM. */

int
inf_read_upto (str, delim)
     sstring *str;
     int delim;
{
  int ch;
  for (;;)
    {
      ch = INF_GET ();
      if (ch == EOF || ch == delim)
	break;
      SSTRING_PUT (str, ch);
    }
  MAKE_SSTRING_SPACE (str, 1);
  *str->ptr = 0;
  return ch;
}

int
inf_scan_ident (s, c)
     register sstring *s;
     int c;
{
  s->ptr = s->base;
  if (isalpha (c) || c == '_')
    {
      for (;;)
	{
	  SSTRING_PUT (s, c);
	  c = INF_GET ();
	  if (c == EOF || !(isalnum (c) || c == '_'))
	    break;
	}
    }
  MAKE_SSTRING_SPACE (s, 1);
  *s->ptr = 0;
  return c;
}

/* Returns 1 if the file is correctly protected against multiple
   inclusion, setting *ifndef_line to the line number of the initial #ifndef
   and setting *endif_line to the final #endif.
   Otherwise return 0. */

int
check_protection (ifndef_line, endif_line)
     int *ifndef_line, *endif_line;
{
  int c;
  int if_nesting = 1; /* Level of nesting of #if's */
  char *protect_name = NULL; /* Identifier following initial #ifndef */
  int define_seen = 0;

  /* Skip initial white space (including comments). */
  for (;; lineno++)
    {
      c = inf_skip_spaces (' ');
      if (c == EOF)
	return 0;
      if (c != '\n')
	break;
    }
  if (c != '#')
    return 0;
  c = inf_scan_ident (&buf, inf_skip_spaces (' '));
  if (SSTRING_LENGTH (&buf) == 0 || strcmp (buf.base, "ifndef") != 0)
    return 0;

  /* So far so good: We've seen an initial #ifndef. */
  *ifndef_line = lineno;
  c = inf_scan_ident (&buf, inf_skip_spaces (c));
  if (SSTRING_LENGTH (&buf) == 0 || c == EOF)
    return 0;
  protect_name = xstrdup (buf.base);

  INF_UNGET (c);
  c = inf_read_upto (&buf, '\n');
  if (c == EOF)
    return 0;
  lineno++;

  for (;;)
    {
      c = inf_skip_spaces (' ');
      if (c == EOF)
	return 0;
      if (c == '\n')
	{
	  lineno++;
	  continue;
	}
      if (c != '#')
	goto skip_to_eol;
      c = inf_scan_ident (&buf, inf_skip_spaces (' '));
      if (SSTRING_LENGTH (&buf) == 0)
	;
      else if (!strcmp (buf.base, "ifndef")
	  || !strcmp (buf.base, "ifdef") || !strcmp (buf.base, "if"))
	{
	  if_nesting++;
	}
      else if (!strcmp (buf.base, "endif"))
	{
	  if_nesting--;
	  if (if_nesting == 0)
	    break;
	}
      else if (!strcmp (buf.base, "else"))
	{
	  if (if_nesting == 1)
	    return 0;
	}
      else if (!strcmp (buf.base, "define"))
	{
	  if (if_nesting != 1)
	    goto skip_to_eol;
	  c = inf_skip_spaces (c);
	  c = inf_scan_ident (&buf, c);
	  if (buf.base[0] > 0 && strcmp (buf.base, protect_name) == 0)
	    define_seen = 1;
	}
    skip_to_eol:
      for (;;)
	{
	  if (c == '\n' || c == EOF)
	    break;
	  c = INF_GET ();
	}
      if (c == EOF)
	return 0;
      lineno++;
    }

  if (!define_seen)
     return 0;
  *endif_line = lineno;
  /* Skip final white space (including comments). */
  for (;;)
    {
      c = inf_skip_spaces (' ');
      if (c == EOF)
	break;
      if (c != '\n')
	return 0;
    }

  return 1;
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  int inf_fd;
  struct stat sbuf;
  int c;
  int i, done;
  char *cptr, *cptr0, **pptr;
  int ifndef_line;
  int endif_line;
  long to_read;
  long int inf_size;

  if (argv[0] && argv[0][0])
    {
      register char *p;

      progname = 0;
      for (p = argv[0]; *p; p++)
        if (*p == '/')
          progname = p;
      progname = progname ? progname+1 : argv[0];
    }

  if (argc < 4)
    {
      fprintf (stderr, "%s: Usage: foo.h infile.h outfile.h req_funcs <scan-file-name\n",
	       progname);
      exit (-1);
    }

  inc_filename = argv[1];
  inc_filename_length = strlen (inc_filename);
  if (strcmp (inc_filename, "sys/stat.h") == 0)
    special_file_handling = sys_stat_special;
  else if (strcmp (inc_filename, "errno.h") == 0)
    special_file_handling = errno_special, missing_errno = 1;

  /* Calculate an upper bound of the number of function names in argv[4] */
  for (i = 1, cptr = argv[4]; *cptr; cptr++)
    if (*cptr == ' ') i++;
  /* Find the list of prototypes required for this include file. */ 
  required_functions = (char**)xmalloc ((i+1) * sizeof (char*));
  for (cptr = argv[4], cptr0 = cptr, pptr = required_functions, done = 0; 
       !done; cptr++)
    {
      done = *cptr == '\0';
      if (*cptr == ' ' || done)
	{
	  *cptr = '\0';
	  if (cptr > cptr0)
	    {
	      struct fn_decl *fn = lookup_std_proto (cptr0);
	      *pptr++ = cptr0;
	      if (fn == NULL)
		fprintf (stderr, "Internal error:  No prototype for %s\n",
			 cptr0);
	      else
		SET_REQUIRED (fn);
	    }
	  cptr0 = cptr + 1;
	}
    }
  required_unseen_count = pptr - required_functions;
  *pptr = 0;

  read_scan_file (stdin);

  inf_fd = open (argv[2], O_RDONLY, 0666);
  if (inf_fd < 0)
    {
      fprintf (stderr, "%s: Cannot open '%s' for reading -",
	       progname, argv[2]);
      perror (NULL);
      exit (-1);
    }
  if (fstat (inf_fd, &sbuf) < 0)
    {
      fprintf (stderr, "%s: Cannot get size of '%s' -", progname, argv[2]);
      perror (NULL);
      exit (-1);
    }
  inf_size = sbuf.st_size;
  inf_buffer = (char*) xmalloc (inf_size + 2);
  inf_buffer[inf_size] = '\n';
  inf_buffer[inf_size + 1] = '\0';
  inf_limit = inf_buffer + inf_size;
  inf_ptr = inf_buffer;

  to_read = inf_size;
  while (to_read > 0)
    {
      long i = read (inf_fd, inf_buffer + inf_size - to_read, to_read);
      if (i < 0)
	{
	  fprintf (stderr, "%s: Failed to read '%s' -", progname, argv[2]);
	  perror (NULL);
	  exit (-1);
	}
      if (i == 0)
	{
	  inf_size -= to_read;
	  break;
	}
      to_read -= i;
    }

  close (inf_fd);

  /* If file doesn't end with '\n', add one. */
  if (inf_limit > inf_buffer && inf_limit[-1] != '\n')
    inf_limit++;

  unlink (argv[3]);
  outf = fopen (argv[3], "w");
  if (outf == NULL)
    {
      fprintf (stderr, "%s: Cannot open '%s' for writing -",
	       progname, argv[3]);
      perror (NULL);
      exit (-1);
    }

  lineno = 1;

  if (check_protection (&ifndef_line, &endif_line))
    {
#if 0
      fprintf (stderr, "#ifndef %s on line %d; #endif on line %d\n",
	       protect_name, ifndef_line, endif_line);
#endif
      lbrac_line = ifndef_line+1;
      rbrac_line = endif_line;
    }
  else
    {
      lbrac_line = 1;
      rbrac_line = -1;
    }

  /* Reset input file. */
  inf_ptr = inf_buffer;
  lineno = 1;

  for (;;)
    {
      if (lineno == lbrac_line)
	write_lbrac ();
      if (lineno == rbrac_line)
	write_rbrac ();
      for (;;)
	{
	  struct fn_decl *fn;
	  c = INF_GET ();
	  if (c == EOF)
	    break;
	  if (isalpha (c) || c == '_')
	    {
	      c = inf_scan_ident (&buf, c);
	      INF_UNGET (c);
	      fputs (buf.base, outf);
	      fn = lookup_std_proto (buf.base);
	      /* We only want to edit the declaration matching the one
		 seen by scan-decls, as there can be multiple
		 declarations, selected by #ifdef __STDC__ or whatever. */
	      if (fn && fn->partial && fn->partial->line_seen == lineno)
		{
		  c = inf_skip_spaces (' ');
		  if (c == EOF)
		    break;
		  if (c == '(')
		    {
		      c = inf_skip_spaces (' ');
		      if (c == ')')
			{
			  fprintf (outf, " _PARAMS((%s))", fn->params);
			}
		      else
			{
			  putc ('(', outf);
			  INF_UNGET (c);
			}
		    }
		  else
		    fprintf (outf, " %c", c);
		}
	    }
	  else
	    {
	      putc (c, outf);
	      if (c == '\n')
		break;
	    }
	}
      if (c == EOF)
	break;
      lineno++;
    }
  if (rbrac_line < 0)
    write_rbrac ();

  fclose (outf);

  return 0;
}
