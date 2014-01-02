/* Copyright (C) 2008-2014 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "version.h"
#include "flags.h"


#include "options.h"
#include "gfortran.h"
#include "tm_p.h"		/* Target prototypes.  */
#include "target.h"
#include "toplev.h"
#include "diagnostic.h"

#include "../../libcpp/internal.h"
#include "cpp.h"
#include "incpath.h"
#include "cppbuiltin.h"
#include "mkdeps.h"

#ifndef TARGET_SYSTEM_ROOT
# define TARGET_SYSTEM_ROOT NULL
#endif

#ifndef TARGET_CPU_CPP_BUILTINS
# define TARGET_CPU_CPP_BUILTINS()
#endif

#ifndef TARGET_OS_CPP_BUILTINS
# define TARGET_OS_CPP_BUILTINS()
#endif

#ifndef TARGET_OBJFMT_CPP_BUILTINS
# define TARGET_OBJFMT_CPP_BUILTINS()
#endif


/* Holds switches parsed by gfc_cpp_handle_option (), but whose
   handling is deferred to gfc_cpp_init ().  */
typedef struct
{
    enum opt_code code;
    const char *arg;
}
gfc_cpp_deferred_opt_t;


/* Defined and undefined macros being queued for output with -dU at
   the next newline.  */
typedef struct gfc_cpp_macro_queue
{
  struct gfc_cpp_macro_queue *next;	/* Next macro in the list.  */
  char *macro;				/* The name of the macro if not
					   defined, the full definition if
					   defined.  */
} gfc_cpp_macro_queue;
static gfc_cpp_macro_queue *cpp_define_queue, *cpp_undefine_queue;

struct gfc_cpp_option_data
{
  /* Argument of -cpp, implied by SPEC;
     if NULL, preprocessing disabled.  */
  const char *temporary_filename;

  const char *output_filename;          /* -o <arg>  */
  int preprocess_only;                  /* -E  */
  int discard_comments;                 /* -C  */
  int discard_comments_in_macro_exp;    /* -CC  */
  int print_include_names;              /* -H  */
  int no_line_commands;                 /* -P  */
  char dump_macros;                     /* -d[DMNU]  */
  int dump_includes;                    /* -dI  */
  int working_directory;                /* -fworking-directory  */
  int no_predefined;                    /* -undef */
  int standard_include_paths;           /* -nostdinc */
  int verbose;                          /* -v */
  int deps;                             /* -M */
  int deps_skip_system;                 /* -MM */
  const char *deps_filename;            /* -M[M]D */
  const char *deps_filename_user;       /* -MF <arg> */
  int deps_missing_are_generated;       /* -MG */
  int deps_phony;                       /* -MP */
  int warn_date_time;                   /* -Wdate-time */

  const char *multilib;                 /* -imultilib <dir>  */
  const char *prefix;                   /* -iprefix <dir>  */
  const char *sysroot;                  /* -isysroot <dir>  */

  /* Options whose handling needs to be deferred until the
     appropriate cpp-objects are created:
      -A predicate=answer
      -D <macro>[=<val>]
      -U <macro>  */
  gfc_cpp_deferred_opt_t *deferred_opt;
  int deferred_opt_count;
}
gfc_cpp_option;

/* Structures used with libcpp:  */
static cpp_options *cpp_option = NULL;
static cpp_reader *cpp_in = NULL;

/* Encapsulates state used to convert a stream of cpp-tokens into
   a text file.  */
static struct
{
  FILE *outf;			/* Stream to write to.  */
  const cpp_token *prev;	/* Previous token.  */
  const cpp_token *source;	/* Source token for spacing.  */
  int src_line;			/* Line number currently being written.  */
  unsigned char printed;	/* Nonzero if something output at line.  */
  bool first_time;		/* cb_file_change hasn't been called yet.  */
} print;

/* General output routines.  */
static void scan_translation_unit (cpp_reader *);
static void scan_translation_unit_trad (cpp_reader *);

/* Callback routines for the parser. Most of these are active only
   in specific modes.  */
static void cb_file_change (cpp_reader *, const struct line_map *);
static void cb_line_change (cpp_reader *, const cpp_token *, int);
static void cb_define (cpp_reader *, source_location, cpp_hashnode *);
static void cb_undef (cpp_reader *, source_location, cpp_hashnode *);
static void cb_def_pragma (cpp_reader *, source_location);
static void cb_include (cpp_reader *, source_location, const unsigned char *,
			const char *, int, const cpp_token **);
static void cb_ident (cpp_reader *, source_location, const cpp_string *);
static void cb_used_define (cpp_reader *, source_location, cpp_hashnode *);
static void cb_used_undef (cpp_reader *, source_location, cpp_hashnode *);
static bool cb_cpp_error (cpp_reader *, int, int, location_t, unsigned int,
			  const char *, va_list *)
     ATTRIBUTE_GCC_DIAG(6,0);
void pp_dir_change (cpp_reader *, const char *);

static int dump_macro (cpp_reader *, cpp_hashnode *, void *);
static void dump_queued_macros (cpp_reader *);


static void
cpp_define_builtins (cpp_reader *pfile)
{
  /* Initialize CPP built-ins; '1' corresponds to 'flag_hosted'
     in C, defines __STDC_HOSTED__?!  */
  cpp_init_builtins (pfile, 0);

  /* Initialize GFORTRAN specific builtins.
     These are documented.  */
  define_language_independent_builtin_macros (pfile);
  cpp_define (pfile, "__GFORTRAN__=1");
  cpp_define (pfile, "_LANGUAGE_FORTRAN=1");

  if (gfc_option.gfc_flag_openmp)
    cpp_define (pfile, "_OPENMP=201107");

  /* The defines below are necessary for the TARGET_* macros.

     FIXME:  Note that builtin_define_std() actually is a function
     in c-cppbuiltin.c which uses flags undefined for Fortran.
     Let's skip this for now. If needed, one needs to look into it
     once more.  */

# define builtin_define(TXT) cpp_define (pfile, TXT)
# define builtin_define_std(TXT)
# define builtin_assert(TXT) cpp_assert (pfile, TXT)

  /* FIXME: Pandora's Box
    Using the macros below results in multiple breakages:
     - mingw will fail to compile this file as dependent macros
       assume to be used in c-cppbuiltin.c only. Further, they use
       flags only valid/defined in C (same as noted above).
       [config/i386/mingw32.h, config/i386/cygming.h]
     - other platforms (not as popular) break similarly
       [grep for 'builtin_define_with_int_value' in gcc/config/]

  TARGET_CPU_CPP_BUILTINS ();
  TARGET_OS_CPP_BUILTINS ();
  TARGET_OBJFMT_CPP_BUILTINS (); */

#undef builtin_define
#undef builtin_define_std
#undef builtin_assert
}

bool
gfc_cpp_enabled (void)
{
  return gfc_cpp_option.temporary_filename != NULL;
}

bool
gfc_cpp_preprocess_only (void)
{
  return gfc_cpp_option.preprocess_only;
}

bool
gfc_cpp_makedep (void)
{
  return gfc_cpp_option.deps;
}

void
gfc_cpp_add_dep (const char *name, bool system)
{
  if (!gfc_cpp_option.deps_skip_system || !system)
    deps_add_dep (cpp_get_deps (cpp_in), name);
}

void
gfc_cpp_add_target (const char *name)
{
  deps_add_target (cpp_get_deps (cpp_in), name, 0);
}


const char *
gfc_cpp_temporary_file (void)
{
  return gfc_cpp_option.temporary_filename;
}

void
gfc_cpp_init_options (unsigned int decoded_options_count,
		      struct cl_decoded_option *decoded_options ATTRIBUTE_UNUSED)
{
  /* Do not create any objects from libcpp here. If no
     preprocessing is requested, this would be wasted
     time and effort.

     See gfc_cpp_post_options() instead.  */

  gfc_cpp_option.temporary_filename = NULL;
  gfc_cpp_option.output_filename = NULL;
  gfc_cpp_option.preprocess_only = 0;
  gfc_cpp_option.discard_comments = 1;
  gfc_cpp_option.discard_comments_in_macro_exp = 1;
  gfc_cpp_option.print_include_names = 0;
  gfc_cpp_option.no_line_commands = 0;
  gfc_cpp_option.dump_macros = '\0';
  gfc_cpp_option.dump_includes = 0;
  gfc_cpp_option.working_directory = -1;
  gfc_cpp_option.no_predefined = 0;
  gfc_cpp_option.standard_include_paths = 1;
  gfc_cpp_option.verbose = 0;
  gfc_cpp_option.warn_date_time = 0;
  gfc_cpp_option.deps = 0;
  gfc_cpp_option.deps_skip_system = 0;
  gfc_cpp_option.deps_phony = 0;
  gfc_cpp_option.deps_missing_are_generated = 0;
  gfc_cpp_option.deps_filename = NULL;
  gfc_cpp_option.deps_filename_user = NULL;

  gfc_cpp_option.multilib = NULL;
  gfc_cpp_option.prefix = NULL;
  gfc_cpp_option.sysroot = TARGET_SYSTEM_ROOT;

  gfc_cpp_option.deferred_opt = XNEWVEC (gfc_cpp_deferred_opt_t,
					 decoded_options_count);
  gfc_cpp_option.deferred_opt_count = 0;
}

int
gfc_cpp_handle_option (size_t scode, const char *arg, int value ATTRIBUTE_UNUSED)
{
  int result = 1;
  enum opt_code code = (enum opt_code) scode;

  switch (code)
  {
    default:
      result = 0;
      break;

    case OPT_cpp_:
      gfc_cpp_option.temporary_filename = arg;
      break;

    case OPT_nocpp:
      gfc_cpp_option.temporary_filename = 0L;
      break;

    case OPT_d:
      for ( ; *arg; ++arg)
        switch (*arg)
	{
	  case 'D':
	  case 'M':
	  case 'N':
	  case 'U':
	    gfc_cpp_option.dump_macros = *arg;
	    break;

	  case 'I':
	    gfc_cpp_option.dump_includes = 1;
	    break;
	}
      break;

    case OPT_fworking_directory:
      gfc_cpp_option.working_directory = value;
      break;

    case OPT_idirafter:
      gfc_cpp_add_include_path_after (xstrdup(arg), true);
      break;

    case OPT_imultilib:
      gfc_cpp_option.multilib = arg;
      break;

    case OPT_iprefix:
      gfc_cpp_option.prefix = arg;
      break;

    case OPT_isysroot:
      gfc_cpp_option.sysroot = arg;
      break;

    case OPT_iquote:
    case OPT_isystem:
      gfc_cpp_add_include_path (xstrdup(arg), true);
      break;

    case OPT_nostdinc:
      gfc_cpp_option.standard_include_paths = value;
      break;

    case OPT_o:
      if (!gfc_cpp_option.output_filename)
	gfc_cpp_option.output_filename = arg;
      else
	gfc_fatal_error ("output filename specified twice");
      break;

    case OPT_undef:
      gfc_cpp_option.no_predefined = value;
      break;

    case OPT_v:
      gfc_cpp_option.verbose = value;
      break;

    case OPT_Wdate_time:
      gfc_cpp_option.warn_date_time = value;

    case OPT_A:
    case OPT_D:
    case OPT_U:
      gfc_cpp_option.deferred_opt[gfc_cpp_option.deferred_opt_count].code = code;
      gfc_cpp_option.deferred_opt[gfc_cpp_option.deferred_opt_count].arg = arg;
      gfc_cpp_option.deferred_opt_count++;
      break;

    case OPT_C:
      gfc_cpp_option.discard_comments = 0;
      break;

    case OPT_CC:
      gfc_cpp_option.discard_comments = 0;
      gfc_cpp_option.discard_comments_in_macro_exp = 0;
      break;

    case OPT_E:
      gfc_cpp_option.preprocess_only = 1;
      break;

    case OPT_H:
      gfc_cpp_option.print_include_names = 1;
      break;

    case OPT_MM:
      gfc_cpp_option.deps_skip_system = 1;
      /* fall through */

    case OPT_M:
      gfc_cpp_option.deps = 1;
      break;

    case OPT_MMD:
      gfc_cpp_option.deps_skip_system = 1;
      /* fall through */

    case OPT_MD:
      gfc_cpp_option.deps = 1;
      gfc_cpp_option.deps_filename = arg;
      break;

    case OPT_MF:
      /* If specified multiple times, last one wins.  */
      gfc_cpp_option.deps_filename_user = arg;
      break;

    case OPT_MG:
      gfc_cpp_option.deps_missing_are_generated = 1;
      break;

    case OPT_MP:
      gfc_cpp_option.deps_phony = 1;
      break;

    case OPT_MQ:
    case OPT_MT:
      gfc_cpp_option.deferred_opt[gfc_cpp_option.deferred_opt_count].code = code;
      gfc_cpp_option.deferred_opt[gfc_cpp_option.deferred_opt_count].arg = arg;
      gfc_cpp_option.deferred_opt_count++;
      break;

    case OPT_P:
      gfc_cpp_option.no_line_commands = 1;
      break;
  }

  return result;
}


void
gfc_cpp_post_options (void)
{
  /* Any preprocessing-related option without '-cpp' is considered
     an error.  */
  if (!gfc_cpp_enabled ()
      && (gfc_cpp_preprocess_only ()
	  || gfc_cpp_makedep ()
	  || !gfc_cpp_option.discard_comments
	  || !gfc_cpp_option.discard_comments_in_macro_exp
	  || gfc_cpp_option.print_include_names
	  || gfc_cpp_option.no_line_commands
	  || gfc_cpp_option.dump_macros
	  || gfc_cpp_option.dump_includes))
    gfc_fatal_error("To enable preprocessing, use -cpp");

  if (!gfc_cpp_enabled ())
    return;

  cpp_in = cpp_create_reader (CLK_GNUC89, NULL, line_table);
  gcc_assert (cpp_in);

  /* The cpp_options-structure defines far more flags than those set here.
     If any other is implemented, see c-opt.c (sanitize_cpp_opts) for
     inter-option dependencies that may need to be enforced.  */
  cpp_option = cpp_get_options (cpp_in);
  gcc_assert (cpp_option);

  /* TODO: allow non-traditional modes, e.g. by -cpp-std=...?  */
  cpp_option->traditional = 1;
  cpp_option->cplusplus_comments = 0;

  cpp_option->cpp_pedantic = pedantic;

  cpp_option->dollars_in_ident = gfc_option.flag_dollar_ok;
  cpp_option->discard_comments = gfc_cpp_option.discard_comments;
  cpp_option->discard_comments_in_macro_exp = gfc_cpp_option.discard_comments_in_macro_exp;
  cpp_option->print_include_names = gfc_cpp_option.print_include_names;
  cpp_option->preprocessed = gfc_option.flag_preprocessed;
  cpp_option->warn_date_time = gfc_cpp_option.warn_date_time;

  if (gfc_cpp_makedep ())
    {
      cpp_option->deps.style = DEPS_USER;
      cpp_option->deps.phony_targets = gfc_cpp_option.deps_phony;
      cpp_option->deps.missing_files = gfc_cpp_option.deps_missing_are_generated;

      /* -MF <arg> overrides -M[M]D.  */
      if (gfc_cpp_option.deps_filename_user)
	gfc_cpp_option.deps_filename = gfc_cpp_option.deps_filename_user;
  }

  if (gfc_cpp_option.working_directory == -1)
    gfc_cpp_option.working_directory = (debug_info_level != DINFO_LEVEL_NONE);

  cpp_post_options (cpp_in);

  gfc_cpp_register_include_paths ();
}


void
gfc_cpp_init_0 (void)
{
  struct cpp_callbacks *cb;

  cb = cpp_get_callbacks (cpp_in);
  cb->file_change = cb_file_change;
  cb->line_change = cb_line_change;
  cb->ident = cb_ident;
  cb->def_pragma = cb_def_pragma;
  cb->error = cb_cpp_error;

  if (gfc_cpp_option.dump_includes)
    cb->include = cb_include;

  if ((gfc_cpp_option.dump_macros == 'D')
      || (gfc_cpp_option.dump_macros == 'N'))
    {
      cb->define = cb_define;
      cb->undef  = cb_undef;
    }

  if (gfc_cpp_option.dump_macros == 'U')
    {
      cb->before_define = dump_queued_macros;
      cb->used_define = cb_used_define;
      cb->used_undef = cb_used_undef;
    }

  /* Initialize the print structure.  Setting print.src_line to -1 here is
     a trick to guarantee that the first token of the file will cause
     a linemarker to be output by maybe_print_line.  */
  print.src_line = -1;
  print.printed = 0;
  print.prev = 0;
  print.first_time = 1;

  if (gfc_cpp_preprocess_only ())
    {
      if (gfc_cpp_option.output_filename)
	{
	  /* This needs cheating: with "-E -o <file>", the user wants the
	     preprocessed output in <file>. However, if nothing is done
	     about it <file> is also used for assembler output. Hence, it
	     is necessary to redirect assembler output (actually nothing
	     as -E implies -fsyntax-only) to another file, otherwise the
	     output from preprocessing is lost.  */
	  asm_file_name = gfc_cpp_option.temporary_filename;

	  print.outf = fopen (gfc_cpp_option.output_filename, "w");
	  if (print.outf == NULL)
	    gfc_fatal_error ("opening output file %s: %s",
			     gfc_cpp_option.output_filename,
			     xstrerror (errno));
	}
      else
	print.outf = stdout;
    }
  else
    {
      print.outf = fopen (gfc_cpp_option.temporary_filename, "w");
      if (print.outf == NULL)
	gfc_fatal_error ("opening output file %s: %s",
			 gfc_cpp_option.temporary_filename, xstrerror (errno));
    }

  gcc_assert(cpp_in);
  if (!cpp_read_main_file (cpp_in, gfc_source_file))
    errorcount++;
}

void
gfc_cpp_init (void)
{
  int i;

  if (gfc_option.flag_preprocessed)
    return;

  if (!gfc_cpp_option.no_predefined)
    {
      /* Make sure all of the builtins about to be declared have
	BUILTINS_LOCATION has their source_location.  */
      source_location builtins_loc = BUILTINS_LOCATION;
      cpp_force_token_locations (cpp_in, &builtins_loc);

      cpp_define_builtins (cpp_in);

      cpp_stop_forcing_token_locations (cpp_in);
    }

  /* Handle deferred options from command-line.  */
  cpp_change_file (cpp_in, LC_RENAME, _("<command-line>"));

  for (i = 0; i < gfc_cpp_option.deferred_opt_count; i++)
    {
      gfc_cpp_deferred_opt_t *opt = &gfc_cpp_option.deferred_opt[i];

      if (opt->code == OPT_D)
	cpp_define (cpp_in, opt->arg);
      else if (opt->code == OPT_U)
	cpp_undef (cpp_in, opt->arg);
      else if (opt->code == OPT_A)
	{
	  if (opt->arg[0] == '-')
	    cpp_unassert (cpp_in, opt->arg + 1);
	  else
	    cpp_assert (cpp_in, opt->arg);
	}
      else if (opt->code == OPT_MT || opt->code == OPT_MQ)
	deps_add_target (cpp_get_deps (cpp_in),
			 opt->arg, opt->code == OPT_MQ);
    }

  if (gfc_cpp_option.working_directory
      && gfc_cpp_option.preprocess_only && !gfc_cpp_option.no_line_commands)
    pp_dir_change (cpp_in, get_src_pwd ());
}

bool
gfc_cpp_preprocess (const char *source_file)
{
  if (!gfc_cpp_enabled ())
    return false;

  cpp_change_file (cpp_in, LC_RENAME, source_file);

  if (cpp_option->traditional)
    scan_translation_unit_trad (cpp_in);
  else
    scan_translation_unit (cpp_in);

  /* -dM command line option.  */
  if (gfc_cpp_preprocess_only () &&
      gfc_cpp_option.dump_macros == 'M')
    {
      putc ('\n', print.outf);
      cpp_forall_identifiers (cpp_in, dump_macro, NULL);
    }

  putc ('\n', print.outf);

  if (!gfc_cpp_preprocess_only ()
      || (gfc_cpp_preprocess_only () && gfc_cpp_option.output_filename))
    fclose (print.outf);

  return true;
}

void
gfc_cpp_done (void)
{
  if (!gfc_cpp_enabled ())
    return;

  gcc_assert (cpp_in);

  if (gfc_cpp_makedep ())
    {
      if (gfc_cpp_option.deps_filename)
	{
	  FILE *f = fopen (gfc_cpp_option.deps_filename, "w");
	  if (f)
	    {
	      cpp_finish (cpp_in, f);
	      fclose (f);
	    }
	  else
	    gfc_fatal_error ("opening output file %s: %s",
			     gfc_cpp_option.deps_filename,
			     xstrerror (errno));
	}
      else
	cpp_finish (cpp_in, stdout);
    }

  cpp_undef_all (cpp_in);
  cpp_clear_file_cache (cpp_in);
}

/* PATH must be malloc-ed and NULL-terminated.  */
void
gfc_cpp_add_include_path (char *path, bool user_supplied)
{
  /* CHAIN sets cpp_dir->sysp which differs from 0 if PATH is a system
     include path. Fortran does not define any system include paths.  */
  int cxx_aware = 0;

  add_path (path, BRACKET, cxx_aware, user_supplied);
}

void
gfc_cpp_add_include_path_after (char *path, bool user_supplied)
{
  int cxx_aware = 0;
  add_path (path, AFTER, cxx_aware, user_supplied);
}

void
gfc_cpp_register_include_paths (void)
{
  int cxx_stdinc = 0;
  register_include_chains (cpp_in, gfc_cpp_option.sysroot,
			   gfc_cpp_option.prefix, gfc_cpp_option.multilib,
			   gfc_cpp_option.standard_include_paths, cxx_stdinc,
			   gfc_cpp_option.verbose);
}



static void scan_translation_unit_trad (cpp_reader *);
static void account_for_newlines (const unsigned char *, size_t);
static int dump_macro (cpp_reader *, cpp_hashnode *, void *);

static void print_line (source_location, const char *);
static void maybe_print_line (source_location);


/* Writes out the preprocessed file, handling spacing and paste
   avoidance issues.  */
static void
scan_translation_unit (cpp_reader *pfile)
{
  bool avoid_paste = false;

  print.source = NULL;
  for (;;)
    {
      const cpp_token *token = cpp_get_token (pfile);

      if (token->type == CPP_PADDING)
	{
	  avoid_paste = true;
	  if (print.source == NULL
	      || (!(print.source->flags & PREV_WHITE)
		  && token->val.source == NULL))
	    print.source = token->val.source;
	  continue;
	}

      if (token->type == CPP_EOF)
	break;

      /* Subtle logic to output a space if and only if necessary.  */
      if (avoid_paste)
	{
	  if (print.source == NULL)
	    print.source = token;
	  if (print.source->flags & PREV_WHITE
	      || (print.prev
		  && cpp_avoid_paste (pfile, print.prev, token))
	      || (print.prev == NULL && token->type == CPP_HASH))
	    putc (' ', print.outf);
	}
      else if (token->flags & PREV_WHITE)
	putc (' ', print.outf);

      avoid_paste = false;
      print.source = NULL;
      print.prev = token;
      cpp_output_token (token, print.outf);

      if (token->type == CPP_COMMENT)
	account_for_newlines (token->val.str.text, token->val.str.len);
    }
}

/* Adjust print.src_line for newlines embedded in output.  */
static void
account_for_newlines (const unsigned char *str, size_t len)
{
  while (len--)
    if (*str++ == '\n')
      print.src_line++;
}

/* Writes out a traditionally preprocessed file.  */
static void
scan_translation_unit_trad (cpp_reader *pfile)
{
  while (_cpp_read_logical_line_trad (pfile))
    {
      size_t len = pfile->out.cur - pfile->out.base;
      maybe_print_line (pfile->out.first_line);
      fwrite (pfile->out.base, 1, len, print.outf);
      print.printed = 1;
      if (!CPP_OPTION (pfile, discard_comments))
	account_for_newlines (pfile->out.base, len);
    }
}

/* If the token read on logical line LINE needs to be output on a
   different line to the current one, output the required newlines or
   a line marker.  */
static void
maybe_print_line (source_location src_loc)
{
  const struct line_map *map = linemap_lookup (line_table, src_loc);
  int src_line = SOURCE_LINE (map, src_loc);

  /* End the previous line of text.  */
  if (print.printed)
    {
      putc ('\n', print.outf);
      print.src_line++;
      print.printed = 0;
    }

  if (src_line >= print.src_line && src_line < print.src_line + 8)
    {
      while (src_line > print.src_line)
	{
	  putc ('\n', print.outf);
	  print.src_line++;
	}
    }
  else
    print_line (src_loc, "");
}

/* Output a line marker for logical line LINE.  Special flags are "1"
   or "2" indicating entering or leaving a file.  */
static void
print_line (source_location src_loc, const char *special_flags)
{
  /* End any previous line of text.  */
  if (print.printed)
    putc ('\n', print.outf);
  print.printed = 0;

  if (!gfc_cpp_option.no_line_commands)
    {
      expanded_location loc;
      size_t to_file_len;
      unsigned char *to_file_quoted;
      unsigned char *p;
      int sysp;

      loc = expand_location (src_loc);
      to_file_len = strlen (loc.file);
      to_file_quoted = (unsigned char *) alloca (to_file_len * 4 + 1);

      print.src_line = loc.line;

      /* cpp_quote_string does not nul-terminate, so we have to do it
	 ourselves.  */
      p = cpp_quote_string (to_file_quoted,
			    (const unsigned char *) loc.file, to_file_len);
      *p = '\0';
      fprintf (print.outf, "# %u \"%s\"%s",
	       print.src_line == 0 ? 1 : print.src_line,
	       to_file_quoted, special_flags);

      sysp = in_system_header_at (src_loc);
      if (sysp == 2)
	fputs (" 3 4", print.outf);
      else if (sysp == 1)
	fputs (" 3", print.outf);

      putc ('\n', print.outf);
    }
}

static void
cb_file_change (cpp_reader * ARG_UNUSED (pfile), const struct line_map *map)
{
  const char *flags = "";

  if (gfc_cpp_option.no_line_commands)
    return;

  if (!map)
    return;

      if (print.first_time)
	{
	  /* Avoid printing foo.i when the main file is foo.c.  */
	  if (!cpp_get_options (cpp_in)->preprocessed)
	    print_line (map->start_location, flags);
	  print.first_time = 0;
	}
      else
	{
	  /* Bring current file to correct line when entering a new file.  */
	  if (map->reason == LC_ENTER)
	    {
	      const struct line_map *from = INCLUDED_FROM (line_table, map);
	      maybe_print_line (LAST_SOURCE_LINE_LOCATION (from));
	    }
	  if (map->reason == LC_ENTER)
	    flags = " 1";
	  else if (map->reason == LC_LEAVE)
	    flags = " 2";
	  print_line (map->start_location, flags);
	}

}

/* Called when a line of output is started.  TOKEN is the first token
   of the line, and at end of file will be CPP_EOF.  */
static void
cb_line_change (cpp_reader *pfile, const cpp_token *token,
		int parsing_args)
{
  source_location src_loc = token->src_loc;

  if (token->type == CPP_EOF || parsing_args)
    return;

  maybe_print_line (src_loc);
  print.prev = 0;
  print.source = 0;

  /* Supply enough spaces to put this token in its original column,
     one space per column greater than 2, since scan_translation_unit
     will provide a space if PREV_WHITE.  Don't bother trying to
     reconstruct tabs; we can't get it right in general, and nothing
     ought to care.  Some things do care; the fault lies with them.  */
  if (!CPP_OPTION (pfile, traditional))
    {
      const struct line_map *map = linemap_lookup (line_table, src_loc);
      int spaces = SOURCE_COLUMN (map, src_loc) - 2;
      print.printed = 1;

      while (-- spaces >= 0)
	putc (' ', print.outf);
    }
}

static void
cb_ident (cpp_reader *pfile ATTRIBUTE_UNUSED, source_location line,
	  const cpp_string *str)
{
  maybe_print_line (line);
  fprintf (print.outf, "#ident %s\n", str->text);
  print.src_line++;
}

static void
cb_define (cpp_reader *pfile ATTRIBUTE_UNUSED, source_location line,
           cpp_hashnode *node ATTRIBUTE_UNUSED)
{
  maybe_print_line (line);
  fputs ("#define ", print.outf);

  /* 'D' is whole definition; 'N' is name only.  */
  if (gfc_cpp_option.dump_macros == 'D')
    fputs ((const char *) cpp_macro_definition (pfile, node),
	   print.outf);
  else
    fputs ((const char *) NODE_NAME (node), print.outf);

  putc ('\n', print.outf);
  if (LOCATION_LINE (line) != 0)
    print.src_line++;
}

static void
cb_undef (cpp_reader *pfile ATTRIBUTE_UNUSED, source_location line,
	  cpp_hashnode *node)
{
  maybe_print_line (line);
  fprintf (print.outf, "#undef %s\n", NODE_NAME (node));
  print.src_line++;
}

static void
cb_include (cpp_reader *pfile ATTRIBUTE_UNUSED, source_location line,
	    const unsigned char *dir, const char *header, int angle_brackets,
	    const cpp_token **comments)
{
  maybe_print_line (line);
  if (angle_brackets)
    fprintf (print.outf, "#%s <%s>", dir, header);
  else
    fprintf (print.outf, "#%s \"%s\"", dir, header);

  if (comments != NULL)
    {
      while (*comments != NULL)
	{
	  if ((*comments)->flags & PREV_WHITE)
	    putc (' ', print.outf);
	  cpp_output_token (*comments, print.outf);
	  ++comments;
	}
    }

  putc ('\n', print.outf);
  print.src_line++;
}

/* Dump out the hash table.  */
static int
dump_macro (cpp_reader *pfile, cpp_hashnode *node, void *v ATTRIBUTE_UNUSED)
{
  if (node->type == NT_MACRO && !(node->flags & NODE_BUILTIN))
    {
      fputs ("#define ", print.outf);
      fputs ((const char *) cpp_macro_definition (pfile, node),
	     print.outf);
      putc ('\n', print.outf);
      print.src_line++;
    }

  return 1;
}

static void
cb_used_define (cpp_reader *pfile, source_location line ATTRIBUTE_UNUSED,
		cpp_hashnode *node)
{
  gfc_cpp_macro_queue *q;
  q = XNEW (gfc_cpp_macro_queue);
  q->macro = xstrdup ((const char *) cpp_macro_definition (pfile, node));
  q->next = cpp_define_queue;
  cpp_define_queue = q;
}

/* Callback from cpp_error for PFILE to print diagnostics from the
   preprocessor.  The diagnostic is of type LEVEL, with REASON set
   to the reason code if LEVEL is represents a warning, at location
   LOCATION, with column number possibly overridden by COLUMN_OVERRIDE
   if not zero; MSG is the translated message and AP the arguments.
   Returns true if a diagnostic was emitted, false otherwise.  */

static bool
cb_cpp_error (cpp_reader *pfile ATTRIBUTE_UNUSED, int level, int reason,
	      location_t location, unsigned int column_override,
	      const char *msg, va_list *ap)
{
  diagnostic_info diagnostic;
  diagnostic_t dlevel;
  bool save_warn_system_headers = global_dc->dc_warn_system_headers;
  bool ret;

  switch (level)
    {
    case CPP_DL_WARNING_SYSHDR:
      global_dc->dc_warn_system_headers = 1;
      /* Fall through.  */
    case CPP_DL_WARNING:
      dlevel = DK_WARNING;
      break;
    case CPP_DL_PEDWARN:
      dlevel = DK_PEDWARN;
      break;
    case CPP_DL_ERROR:
      dlevel = DK_ERROR;
      break;
    case CPP_DL_ICE:
      dlevel = DK_ICE;
      break;
    case CPP_DL_NOTE:
      dlevel = DK_NOTE;
      break;
    case CPP_DL_FATAL:
      dlevel = DK_FATAL;
      break;
    default:
      gcc_unreachable ();
    }
  diagnostic_set_info_translated (&diagnostic, msg, ap,
				  location, dlevel);
  if (column_override)
    diagnostic_override_column (&diagnostic, column_override);
  if (reason == CPP_W_WARNING_DIRECTIVE)
    diagnostic_override_option_index (&diagnostic, OPT_Wcpp);
  ret = report_diagnostic (&diagnostic);
  if (level == CPP_DL_WARNING_SYSHDR)
    global_dc->dc_warn_system_headers = save_warn_system_headers;
  return ret;
}

/* Callback called when -fworking-director and -E to emit working
   directory in cpp output file.  */

void
pp_dir_change (cpp_reader *pfile ATTRIBUTE_UNUSED, const char *dir)
{
  size_t to_file_len = strlen (dir);
  unsigned char *to_file_quoted =
     (unsigned char *) alloca (to_file_len * 4 + 1);
  unsigned char *p;

  /* cpp_quote_string does not nul-terminate, so we have to do it ourselves.  */
  p = cpp_quote_string (to_file_quoted, (const unsigned char *) dir, to_file_len);
  *p = '\0';
  fprintf (print.outf, "# 1 \"%s//\"\n", to_file_quoted);
}

/* Copy a #pragma directive to the preprocessed output.  */
static void
cb_def_pragma (cpp_reader *pfile, source_location line)
{
  maybe_print_line (line);
  fputs ("#pragma ", print.outf);
  cpp_output_line (pfile, print.outf);
  print.src_line++;
}

static void
cb_used_undef (cpp_reader *pfile ATTRIBUTE_UNUSED,
	       source_location line ATTRIBUTE_UNUSED,
	       cpp_hashnode *node)
{
  gfc_cpp_macro_queue *q;
  q = XNEW (gfc_cpp_macro_queue);
  q->macro = xstrdup ((const char *) NODE_NAME (node));
  q->next = cpp_undefine_queue;
  cpp_undefine_queue = q;
}

static void
dump_queued_macros (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  gfc_cpp_macro_queue *q;

  /* End the previous line of text.  */
  if (print.printed)
    {
      putc ('\n', print.outf);
      print.src_line++;
      print.printed = 0;
    }

  for (q = cpp_define_queue; q;)
    {
      gfc_cpp_macro_queue *oq;
      fputs ("#define ", print.outf);
      fputs (q->macro, print.outf);
      putc ('\n', print.outf);
      print.src_line++;
      oq = q;
      q = q->next;
      free (oq->macro);
      free (oq);
    }
  cpp_define_queue = NULL;
  for (q = cpp_undefine_queue; q;)
    {
      gfc_cpp_macro_queue *oq;
      fprintf (print.outf, "#undef %s\n", q->macro);
      print.src_line++;
      oq = q;
      q = q->next;
      free (oq->macro);
      free (oq);
    }
  cpp_undefine_queue = NULL;
}
