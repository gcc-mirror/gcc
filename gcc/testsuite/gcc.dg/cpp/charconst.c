/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */

/* This tests various diagnostics about character constants, for both
   the preprocessor and the compiler.

   Neil Booth, 22 May 2001.  */

#if ''			/* { dg-warning "empty" "empty charconst" } */
#endif
#if L''			/* { dg-warning "empty" "empty wide charconst" } */
#endif
#if 'very long'		/* { dg-warning "too long" "long charconst" } */
#endif
#if L'very long'	/* { dg-warning "too long" "long wide charconst" } */
#endif
/* Don't do this test for L'ab'; it depends upon sizeof (wchar_t).  */
#if 'ab'		/* { dg-warning "multi-char" "multi-character" } */
#endif

void foo ()
{
  int c = '';		/* { dg-warning "empty" "empty charconst" } */
  c = L'';		/* { dg-warning "empty" "empty wide charconst" } */

  c = 'very long';	/* { dg-warning "too long" "long charconst" } */
  c = L'very long';	/* { dg-warning "too long" "long wide charconst" } */

  /* Don't do this test for L'ab'; it depends upon sizeof (wchar_t).  */
  c = 'ab';		/* { dg-warning "multi-char" "multi-character" } */
 
}
