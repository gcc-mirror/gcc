
/*

   Test to see if a particular fix should be applied to a header file.

   Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.

= = = = = = = = = = = = = = = = = = = = = = = = =

NOTE TO DEVELOPERS

The routines you write here must work closely with both the fixincl.c
and the test_need.c program.

Here are the rules:

1.  Every test procedure name must be suffixed with "_test".
    These routines will be referenced from inclhack.def, sans the suffix.

2.  Use the "TEST_FOR_FIX_PROC_HEAD()" macro _with_ the "_test" suffix
    (I cannot use the ## magic from ANSI C) for defining your entry point.

3.  Put your test name into the FIX_TEST_TABLE

4.  Do not write anything to stdout.  It may be closed.

5.  Write to stderr only in the event of a reportable error
    In such an event, call "exit(1)".

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

#define SHOULD_APPLY(afp) ((afp) == APPLY_FIX)
apply_fix_p_t run_test();

typedef struct {
    tCC*  test_name;
    apply_fix_p_t (*test_proc)();
} test_entry_t;

#define FIX_TEST_TABLE \
  _FT_( "double_slash",     double_slash_test ) \
  _FT_( "else_endif_label", else_endif_label_test )


#define TEST_FOR_FIX_PROC_HEAD( test ) \
static apply_fix_p_t test ( fname, text ) \
    tCC* fname; \
    tCC* text;

/*
 *  Skip over a quoted string.  Single quote strings may
 *  contain multiple characters if the first character is
 *  a backslash.  Especially a backslash followed by octal digits.
 *  We are not doing a correctness syntax check here.
 */
tSCC*
skip_quote( q, text )
  char  q;
  char* text;
{
  for (;;)
    {
      char ch = *(text++);
      switch (ch)
        {
        case '\\':
          text++; /* skip over whatever character follows */
          break;

        case '"':
        case '\'':
          if (ch != q)
            break;
          /*FALLTHROUGH*/

        case '\n':
        case NUL:
          goto skip_done;
        }
    } skip_done:;

  return text;
}


TEST_FOR_FIX_PROC_HEAD( double_slash_test )
{
  if (is_cxx_header (fname, text))
    return SKIP_FIX;

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
                The fix must be applied
              */
              return APPLY_FIX;

            case '*':
              /* We found a C-style comment.  Skip forward to the end */
              text = strstr( text+1, "*/" );
              if (text == (char*)NULL)
                goto test_done;
              text += 2;
            }
          break;

        case NUL:
          goto test_done;

        case '"':
        case '\'':
          text = skip_quote( ch, text );
        }

    } test_done:;

  return SKIP_FIX;
}


TEST_FOR_FIX_PROC_HEAD( else_endif_label_test )
{
  static int compiled = 0;
  tSCC label_pat[] = "^[ \t]*#[ \t]*(else|endif)";
  static regex_t label_re;

  char ch;
  tCC* pz_next = (char*)NULL;
  regmatch_t match[2];
  t_bool file_is_cxx = is_cxx_header( fname, text );

  /*
     This routine may be run many times within a single execution.
     Do the compile once only in that case.  In the standalone case,
     we waste 10 bytes of memory and a test, branch and increment delay.  */
  if (! compiled)
    {
      compiled++;
      re_set_syntax (RE_SYNTAX_EGREP);
      (void)re_compile_pattern (label_pat, sizeof (label_pat)-1,
                                &label_re);
    }

  for (;;) /* entire file */
    {
      /*
        See if we need to advance to the next candidate directive
        If the scanning pointer passes over the end of the directive,
        then the directive is inside a comment */
      if (pz_next < text)
        {
          if (regexec (&label_re, text, 2, match, 0) != 0)
            break;
          pz_next = text + match[0].rm_eo;
        }

      /*
        IF the scan pointer has not reached the directive end, ... */
      if (pz_next > text)
        {
          /*
            Advance the scanning pointer.  If we are at the start
            of a quoted string or a comment, then skip the entire unit */
          ch = *(text++);

          switch (ch)
            {
            case '/':
              /*
                Skip comments */
              if (*text == '*')
                {
                  text = strstr( text+1, "*/" );
                  if (text == (char*)NULL)
                    return SKIP_FIX;
                  text += 2;
                  continue;
                }
              break;

            case '"':
            case '\'':
              text = skip_quote( ch, text );
              break;
            } /* switch (ch) */
          continue;
        } /* if (still shy of directive end) */

      /*
         The scanning pointer (text) has reached the end of the current
         directive under test, then check for bogons here */
      for (;;) /* bogon check */
        {
          char ch = *(pz_next++);
          if (isspace (ch))
            {
              if (ch == '\n')
                {
                  /*
                    It is clean.  No bogons on this directive */
                  text = pz_next;
                  pz_next = (char*)NULL; /* force a new regex search */
                  break;
                }
              continue;
            }

          switch (ch)
            {
            case '\\':
              /*
                Skip escaped newlines.  Otherwise, we have a bogon */
              if (*pz_next != '\n')
                return APPLY_FIX;

              pz_next++;
              break;

            case '/':
              /*
                Skip comments.  Otherwise, we have a bogon */
              switch (*pz_next)
                {
                case '/':
                  /* IF we found a "//" in a C header, THEN fix it. */
                  if (! file_is_cxx)
                    return APPLY_FIX;

                  /* C++ header.  Skip to newline and continue. */
                  pz_next = strchr( pz_next+1, '\n' );
                  if (pz_next == (char*)NULL)
                    return SKIP_FIX;
                  pz_next++;
                  break;

                case '*':
                  /* A comment for either C++ or C.  Skip over it. */
                  pz_next = strstr( pz_next+1, "*/" );
                  if (pz_next == (char*)NULL)
                    return SKIP_FIX;
                  pz_next += 2;
                  break;

                default:
                  /* a '/' followed by other junk. */
                  return APPLY_FIX;
                }
              break; /* a C or C++ comment */

            default:
              /*
                GOTTA BE A BOGON */
              return APPLY_FIX;
            } /* switch (ch) */
        } /* for (bogon check loop) */
    } /* for (entire file) loop */

  return SKIP_FIX;
}

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

     test for fix selector

     THIS IS THE ONLY EXPORTED ROUTINE

*/
apply_fix_p_t
run_test( tname, fname, text )
  tCC* tname;
  tCC* fname;
  tCC* text;
{
#define _FT_(n,p) { n, p },
  static test_entry_t test_table[] = { FIX_TEST_TABLE { NULL, NULL }};
#undef _FT_
#define TEST_TABLE_CT ((sizeof(test_table)/sizeof(test_table[0]))-1)

  int ct = TEST_TABLE_CT;
  test_entry_t* pte = test_table;

  do
    {
      if (strcmp( pte->test_name, tname ) == 0)
        return (*pte->test_proc)( fname, text );
      pte++;
    } while (--ct > 0);
  fprintf( stderr, "fixincludes error:  the `%s' fix test is unknown\n",
           tname );
  exit( 3 );
}

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

     MAIN ROUTINE

     This file is both included in fixincl.c and compiled as a separate
     program for use by the inclhack.sh script.

*/

#ifdef MAIN

int
main( argc, argv )
  int argc;
  char** argv;
{
  char* fname = *++argv;
  char* tname = *++argv;
  char* buf;

  if (argc != 3)
    return run_test( "No test name provided", NULL, NULL, 0 );

  fclose( stdin );
  fclose( stdout );

  buf = load_file_data (fopen (fname, "r"));

  return run_test( tname, fname, buf );
}

#endif
