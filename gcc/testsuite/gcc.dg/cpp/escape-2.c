/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-pedantic -std=c99 -fno-show-column" } */

/* This tests various diagnostics with -pedantic about escape
   sequences, for both the preprocessor and the compiler.

   Neil Booth, 22 May 2001.  */

#if '\e'		/* { dg-warning "non-ISO" "non-ISO \\e" } */
#endif
#if '\u00a0'		/* { dg-bogus "unknown" "\\u is known in C99" } */
#endif

void foo ()
{
  int c = '\E';		/* { dg-warning "non-ISO" "non-ISO \\E" } */
  c = '\u00a0';		/* { dg-bogus "unknown" "\\u is known in C99" } */
}
