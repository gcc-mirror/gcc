/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-Wtraditional -std=c89 -fno-show-column" } */

/* This tests various diagnostics with -Wtraditioanl about escape
   sequences, for both the preprocessor and the compiler.

   Neil Booth, 22 May 2001.  */

#if '\a'		/* { dg-warning "traditional" "traditional bell" } */
#endif
#if '\x1a' != 26	/* { dg-warning "traditional" "traditional hex" } */
 #error bad hex		/* { dg-bogus "bad" "bad hexadecimal evaluation" } */
#endif
#if L'\u00a1'		/* { dg-warning "only valid" "\u is unknown in C89" } */
#endif

void foo ()
{
  int c = '\a';		/* { dg-warning "traditional" "traditional bell" } */

  c = '\xa1';		/* { dg-warning "traditional" "traditional hex" } */
  c = L'\u00a1';	/* { dg-warning "only valid" "\u is unknown in C89" } */
}
