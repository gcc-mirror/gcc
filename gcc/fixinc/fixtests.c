
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

typedef int apply_fix_p_t;  /* Apply Fix Predicate Type */

#define APPLY_FIX 0
#define SKIP_FIX  1

#define SHOULD_APPLY(afp) ((afp) == APPLY_FIX)
apply_fix_p_t run_test();

typedef struct {
    const char*  test_name;
    apply_fix_p_t (*test_proc)();
} test_entry_t;

#define FIX_TEST_TABLE \
  _FT_( "double_slash", double_slash_test )


#define TEST_FOR_FIX_PROC_HEAD( test ) \
static apply_fix_p_t test ( fname, text ) \
    const char* fname; \
    const char* text;

/*
 *  Skip over a quoted string.  Single quote strings may
 *  contain multiple characters if the first character is
 *  a backslash.  Especially a backslash followed by octal digits.
 *  We are not doing a correctness syntax check here.
 */
static const char*
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
  /*  First, check to see if the file is in a C++ directory */
  if (strstr( fname, "CC/" ) != NULL)
    return SKIP_FIX;
  if (strstr( fname, "xx/" ) != NULL)
    return SKIP_FIX;
  if (strstr( fname, "++/" ) != NULL)
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

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

     test for fix selector

     THIS IS THE ONLY EXPORTED ROUTINE

*/
apply_fix_p_t
run_test( tname, fname, text )
  const char* tname;
  const char* fname;
  const char* text;
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
  size_t buf_size = 0;

  if (argc != 3)
    return run_test( "No test name provided", NULL, NULL, 0 );

  fclose( stdin );
  fclose( stdout );

  buf = load_file_data (fopen (fname, "r"));

  return run_test( tname, fname, buf );
}

#endif
