
/*

   Test to see if a particular fix should be applied to a header file.

   Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.

= = = = = = = = = = = = = = = = = = = = = = = = =

NOTE TO DEVELOPERS

The routines you write here must work closely with both the fixincl.c
and the test_need.c program.

Here are the rules:

1.  Every test procedure name must be suffixed with "_fix".
    These routines will be referenced from inclhack.def, sans the suffix.

2.  Use the "FIX_PROC_HEAD()" macro _with_ the "_fix" suffix
    (I cannot use the ## magic from ANSI C) for defining your entry point.

3.  Put your test name into the FIXUP_TABLE

4.  Do not read anything from stdin.  It is closed.

5.  Write to stderr only in the event of a reportable error
    In such an event, call "exit(1)".

6.  If "MAIN" is _not_ defined, then you have access to the fixDescList
    entry for the fix in question.  This may be useful, for example,
    if there are pre-compiled selection expressions stored there.

    For example, you may do this if you know that the first 
    test contains a useful regex.  This is okay because, remember,
    this code perforce works closely with the inclhack.def fixes!!


    tFixDesc*  pMyDesc = fixDescList + MY_FIX_NAME_FIXIDX;
    tTestDesc* pTestList = pMyDesc->p_test_desc;

    regexec (pTestList->p_test_regex, ...)


    If MAIN _is_ defined, then you will have to compile it on
    your own.

= = = = = = = = = = = = = = = = = = = = = = = = =

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "fixlib.h"

typedef struct {
    const char*  fix_name;
    void (*fix_proc)();
} fix_entry_t;

#define FIXUP_TABLE \
  _FT_( "no_double_slash",  double_slash_fix ) \
  _FT_( "else_endif_label", else_endif_label_fix )


#define FIX_PROC_HEAD( fix ) \
static void fix ( filname, text ) \
    const char* filname; \
    char* text;


/*
 *  Skip over a quoted string.  Single quote strings may
 *  contain multiple characters if the first character is
 *  a backslash.  Especially a backslash followed by octal digits.
 *  We are not doing a correctness syntax check here.
 */
static char*
print_quote( q, text )
  char  q;
  char* text;
{
  fputc( q, stdout );

  for (;;)
    {
      char ch = *(text++);
      fputc( ch, stdout );

      switch (ch)
        {
        case '\\':
          if (*text == NUL)
            goto quote_done;

          fputc( *(text++), stdout );
          break;

        case '"':
        case '\'':
          if (ch != q)
            break;
          /*FALLTHROUGH*/

        case '\n':
        case NUL:
          goto quote_done;
        }
    } quote_done:;

  return text;
}


FIX_PROC_HEAD( double_slash_fix )
{
  /*  Now look for the comment markers in the text */
  for (;;)
    {
      char ch = *(text++);
      switch (ch)
        {
        case '/':
          switch (*text) /* do not advance `text' here */
            {
            case '/':
              /*
                We found a "//" pair in open text.
                Delete text to New-Line
              */
              while ((*text != '\n') && (*text != '\0'))  text++;
              break;

            case '*':
              {
                /* We found a C-style comment.  Skip forward to the end */
                char* pz = strstr( (--text)+2, "*/" );
                if (pz == (char*)NULL)
                  {
                    fputs( text, stdout );
                    goto fix_done;
                  }
                pz += 2;
                fwrite (text, (pz - text), 1, stdout );
                text = pz;
              }
              break;

            default:
              fputc (ch, stdout );
            }
          break;

        case NUL:
          goto fix_done;

        case '"':
        case '\'':
          text = print_quote (ch, text );
          break;

        default:
          fputc (ch, stdout );
        }

    } fix_done:;

  fclose (stdout);;
}


FIX_PROC_HEAD( else_endif_label_fix )
{
  static const char label_pat[] = "^[ \t]*#[ \t]*(else|endif)";
  static regex_t label_re;

  char ch;
  char* pz_next = (char*)NULL;
  regmatch_t match[2];

  compile_re (label_pat, &label_re, 1,
	      "label pattern", "else_endif_label_fix");

  for (;;) /* entire file */
    {
      /*
        See if we need to advance to the next candidate directive
        If the scanning pointer passes over the end of the directive,
        then the directive is inside a comment */
      if (pz_next < text)
        {
          if (regexec (&label_re, text, 2, match, 0) != 0)
            {
              fputs( text, stdout );
              break;
            }

          pz_next = text + match[0].rm_eo;
        }

      /*
        IF the scan pointer has not reached the directive end, ... */
      if (pz_next > text)
        {
          /*
            Advance the scanning pointer.  If we are at the start
            of a quoted string or a comment, then skip the entire unit */
          ch = *text;

          switch (ch)
            {
            case '/':
              /*
                Skip comments */
              if (text[1] == '*')
                {
                  char* pz = strstr( text+2, "*/" );
                  if (pz == (char*)NULL)
                    {
                      fputs( text, stdout );
                      return;
                    }
                  pz += 2;
                  fwrite( text, 1, (pz - text), stdout );
                  text = pz;
                  continue;
                }
              putc( ch, stdout );
              text++;
              break;

            case '"':
            case '\'':
              text = print_quote( ch, text+1 );
              break;

            default:
              putc( ch, stdout );
              text++;
            } /* switch (ch) */
          continue;
        } /* if (still shy of directive end) */

      /*
         The scanning pointer (text) has reached the end of the current
         directive under test.  Check for bogons here.  */
      for (;;) /* bogon check */
        {
          char ch = *(text++);
          if (isspace (ch))
            {
              putc( ch, stdout );
              if (ch == '\n')
                {
                  /*
                    It is clean.  No bogons on this directive */
                  pz_next = (char*)NULL; /* force a new regex search */
                  goto dont_fix_bogon;
                }
              continue;
            }

          switch (ch)
            {
            case NUL:
              return;

            case '\\':
              /*
                Skip escaped newlines.  Otherwise, we have a bogon */
              if (*text != '\n') {
                text--;
                goto fix_the_bogon;
              }

              /*
                Emit the escaped newline and keep scanning for possible junk */
              putc( '\\', stdout );
              putc( '\n', stdout );
              text++;
              break;

            case '/':
              /*
                Skip comments.  Otherwise, we have a bogon */
              if (*text == '*')
                {
                  text--;
                  pz_next = strstr( text+2, "*/" );
                  if (pz_next == (char*)NULL)
                    {
                      putc( '\n', stdout );
                      return;
                    }
                  pz_next += 2;
                  fwrite( text, 1, (pz_next - text), stdout );
                  text = pz_next;
                  break;
                }

              /* FALLTHROUGH */

            default:
              /*
                GOTTA BE A BOGON */
              text--;
              goto fix_the_bogon;
            } /* switch (ch) */
        } /* for (bogon check loop) */

    fix_the_bogon:
      /*
        `text' points to the start of the bogus data */
      for (;;)
        {
          /*
            NOT an escaped newline.  Find the end of line that
            is not preceeded by an escape character:  */
          pz_next = strchr( text, '\n' );
          if (pz_next == (char*)NULL)
            {
              putc( '\n', stdout );
              return;
            }

          if (pz_next[-1] != '\\')
            {
              text = pz_next;
              pz_next = (char*)NULL; /* force a new regex search */
              break;
            }

          /*
            The newline was escaped.  We gotta keep going.  */
          text = pz_next + 1;
        }

    dont_fix_bogon:;
    } /* for (entire file) loop */

  return;
}

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

     test for fix selector

     THIS IS THE ONLY EXPORTED ROUTINE

*/
void
apply_fix( fixname, filname )
  const char* fixname;
  const char* filname;
{
#define _FT_(n,p) { n, p },
  static fix_entry_t fix_table[] = { FIXUP_TABLE { NULL, NULL }};
#undef _FT_
#define FIX_TABLE_CT ((sizeof(fix_table)/sizeof(fix_table[0]))-1)

  char* buf;
  int ct = FIX_TABLE_CT;
  fix_entry_t* pfe = fix_table;

  for (;;)
    {
      if (strcmp (pfe->fix_name, fixname) == 0)
        break;
      if (--ct <= 0)
        {
          fprintf (stderr, "fixincludes error:  the `%s' fix is unknown\n",
                   fixname );
          exit (3);
        }
      pfe++;
    }

  buf = load_file_data (stdin);
  (*pfe->fix_proc)( filname, buf );
}

#ifdef MAIN

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

     MAIN ROUTINE

     This file is both included in fixincl.c and compiled as a separate
     program for use by the inclhack.sh script.

*/

int
main( argc, argv )
  int argc;
  char** argv;
{
  if (argc != 3)
    apply_fix ("No test name provided", NULL, NULL, 0 );

  apply_fix (argv[2], argv[1]);
  return 0;
}

#endif
