/* CPP main program, using CPP Library.
   Copyright (C) 1995, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   Written by Per Bothner, 1994-95.

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

const char *progname;

cpp_reader parse_in;
cpp_printer parse_out;

int main		PARAMS ((int, char **));

/* Callback routines for the parser.   Most of these are active only
   in specific modes.  */
static void cb_define	PARAMS ((cpp_reader *, cpp_hashnode *));
static void cb_undef	PARAMS ((cpp_reader *, cpp_hashnode *));
static void cb_include	PARAMS ((cpp_reader *, const unsigned char *,
				 const unsigned char *, unsigned int, int));

static void cb_ident	  PARAMS ((cpp_reader *, const unsigned char *,
				   unsigned int));
static void cb_enter_file PARAMS ((cpp_reader *));
static void cb_leave_file PARAMS ((cpp_reader *));
static void cb_rename_file PARAMS ((cpp_reader *));
static void cb_def_pragma PARAMS ((cpp_reader *));

static void do_pragma_implementation PARAMS ((cpp_reader *));
static int dump_macros_helper PARAMS ((cpp_reader *, cpp_hashnode *));

int
main (argc, argv)
     int argc;
     char **argv;
{
  char *p;
  cpp_reader *pfile = &parse_in;
  cpp_printer *print;
  int argi = 1;  /* Next argument to handle.  */

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && ! IS_DIR_SEPARATOR (p[-1])) --p;
  progname = p;

  xmalloc_set_program_name (progname);

#ifdef HAVE_LC_MESSAGES
  setlocale (LC_MESSAGES, "");
#endif
  (void) bindtextdomain (PACKAGE, localedir);
  (void) textdomain (PACKAGE);

  cpp_init ();
  cpp_reader_init (pfile);
  
  argi += cpp_handle_options (pfile, argc - argi , argv + argi);
  if (argi < argc && ! CPP_FATAL_ERRORS (pfile))
    cpp_fatal (pfile, "Invalid option %s", argv[argi]);
  if (CPP_FATAL_ERRORS (pfile))
    return (FATAL_EXIT_CODE);

  /* Open the output now.  We must do so even if no_output is on,
     because there may be other output than from the actual
     preprocessing (e.g. from -dM).  */
  print = cpp_printer_init (pfile, &parse_out);
  if (! print)
    return (FATAL_EXIT_CODE);

  /* Set callbacks.  */
  if (! CPP_OPTION (pfile, no_line_commands)
      && ! CPP_OPTION (pfile, no_output))
    {
      pfile->cb.enter_file = cb_enter_file;
      pfile->cb.leave_file = cb_leave_file;
      pfile->cb.rename_file = cb_rename_file;
    }
  if (CPP_OPTION (pfile, dump_includes))
    pfile->cb.include  = cb_include;
  if (CPP_OPTION (pfile, debug_output)
      || CPP_OPTION (pfile, dump_macros) == dump_names
      || CPP_OPTION (pfile, dump_macros) == dump_definitions)
    {
      pfile->cb.define = cb_define;
      pfile->cb.undef  = cb_undef;
      pfile->cb.poison = cb_def_pragma;
    }
  pfile->cb.ident      = cb_ident;
  pfile->cb.def_pragma = cb_def_pragma;

  /* Register one #pragma which needs special handling.  */
  cpp_register_pragma(pfile, 0, "implementation", do_pragma_implementation);
  cpp_register_pragma(pfile, "GCC", "implementation", do_pragma_implementation);

  if (! cpp_start_read (pfile, print, CPP_OPTION (pfile, in_fname)))
    return (FATAL_EXIT_CODE);

  if (CPP_OPTION (pfile, no_output))
    while (CPP_BUFFER (pfile) != NULL)
      cpp_scan_buffer_nooutput (pfile);
  else
    while (CPP_BUFFER (pfile) != NULL)
      cpp_scan_buffer (pfile, print);

  if (CPP_OPTION (pfile, dump_macros) == dump_only)
    cpp_forall_identifiers (pfile, dump_macros_helper);
  
  cpp_finish (pfile, print);
  cpp_cleanup (pfile);

  if (parse_in.errors)
    return (FATAL_EXIT_CODE);
  return (SUCCESS_EXIT_CODE);
}

/* Callbacks */

static void
cb_ident (pfile, str, len)
     cpp_reader *pfile;
     const unsigned char *str;
     unsigned int len;
{
  cpp_printf (pfile, &parse_out, "#ident \"%.*s\"\n", (int) len, str);
  parse_out.lineno++;
}

static void
cb_define (pfile, hash)
     cpp_reader *pfile;
     cpp_hashnode *hash;
{
  if (pfile->done_initializing)
    {
      cpp_printf (pfile, &parse_out, "#define %s", hash->name);
      if (CPP_OPTION (pfile, debug_output)
	  || CPP_OPTION (pfile, dump_macros) == dump_definitions)
	cpp_dump_definition (pfile, parse_out.outf, hash);
      putc ('\n', parse_out.outf);
      parse_out.lineno++;
    }
}

static void
cb_undef (pfile, hash)
     cpp_reader *pfile;
     cpp_hashnode *hash;
{
  if (pfile->done_initializing)
    {
      cpp_printf (pfile, &parse_out, "#undef %s\n", hash->name);
      parse_out.lineno++;
    }
}

static void
cb_include (pfile, dir, str, len, ab)
     cpp_reader *pfile;
     const unsigned char *dir;
     const unsigned char *str;
     unsigned int len;
     int ab;
{
  int l, r;
  if (ab)
    l = '<', r = '>';
  else
    l = '"', r = '"';

  cpp_printf (pfile, &parse_out, "#%s %c%.*s%c\n", dir, l, (int) len, str, r);
  parse_out.lineno++;
}

static void
cb_enter_file (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  cpp_printf (pfile, &parse_out, "# 1 \"%s\"%s%s\n", ip->nominal_fname,
	      pfile->done_initializing ? " 1" : "",
	      cpp_syshdr_flags (pfile, ip));

  parse_out.lineno = 1;
  parse_out.last_fname = ip->nominal_fname;
}

static void
cb_leave_file (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  cpp_printf (pfile, &parse_out, "# %u \"%s\" 2%s\n", ip->lineno,
	      ip->nominal_fname, cpp_syshdr_flags (pfile, ip));

  parse_out.lineno = ip->lineno;
  parse_out.last_fname = ip->nominal_fname;
}

static void
cb_rename_file (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  cpp_printf (pfile, &parse_out, "# %u \"%s\"%s\n", ip->lineno,
	      ip->nominal_fname, cpp_syshdr_flags (pfile, ip));

  parse_out.lineno = ip->lineno;
  parse_out.last_fname = ip->nominal_fname;
}

static void
cb_def_pragma (pfile)
     cpp_reader *pfile;
{
  cpp_printf (pfile, &parse_out, "#pragma ");
  cpp_output_list (pfile, parse_out.outf, &pfile->token_list,
		   pfile->first_directive_token + 2);
  putc ('\n', parse_out.outf);
  parse_out.lineno++;
}

static void
do_pragma_implementation (pfile)
     cpp_reader *pfile;
{
  /* Be quiet about `#pragma implementation' for a file only if it hasn't
     been included yet.  */
  const cpp_token *tok = cpp_get_token (pfile);
  char *copy;

  if (tok->type != CPP_EOF)
    {
      if (tok->type != CPP_STRING || cpp_get_token (pfile)->type != CPP_EOF)
	{
	  cpp_error (pfile, "malformed #pragma implementation");
	  return;
	}

      /* Make a NUL-terminated copy of the string.  */
      copy = alloca (tok->val.str.len + 1);
      memcpy (copy, tok->val.str.text, tok->val.str.len);
      copy[tok->val.str.len] = '\0';
  
      if (cpp_included (pfile, copy))
	cpp_warning (pfile,
		"#pragma implementation for %s appears after file is included",
		     copy);
    }

  /* forward to default-pragma handler.  */
  cb_def_pragma (pfile);
}

/* Dump out the hash table.  */
static int
dump_macros_helper (pfile, hp)
     cpp_reader *pfile;
     cpp_hashnode *hp;
{
  if (hp->type == T_MACRO)
    {
      cpp_printf (pfile, &parse_out, "#define %s", hp->name);
      cpp_dump_definition (pfile, parse_out.outf, hp);
      putc ('\n', parse_out.outf);
      parse_out.lineno++;
    }

  return 1;
}

