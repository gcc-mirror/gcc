/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */

/* This tests various diagnostics about character constants, for both
   the preprocessor and the compiler.

   Neil Booth, 22 May 2001.  */

#if ''			/* { dg-error "empty" "empty charconst" } */
#endif
#if L''			/* { dg-error "empty" "empty wide charconst" } */
#endif
#if 'very long'		/* { dg-warning "multi-character literal with \[0-9]+ characters exceeds 'int' size of \[0-9]+ bytes" "long charconst" } */
#endif
#if L'very long'	/* { dg-warning "multi-character literal cannot have an encoding prefix" "long wide charconst" } */
#endif
/* Don't do this test for L'ab'; it depends upon sizeof (wchar_t).  */
#if 'ab'		/* { dg-warning "multi-char" "multi-character" } */
#endif

void foo ()
{
  int c;
  __WCHAR_TYPE__ w;

  c = '';		/* { dg-error "empty" "empty charconst" } */
  w = L'';		/* { dg-error "empty" "empty wide charconst" } */

  c = 'very long';	/* { dg-warning "multi-character literal with \[0-9]+ characters exceeds 'int' size of \[0-9]+ bytes" "long charconst" } */
  w = L'very long';	/* { dg-warning "multi-character literal cannot have an encoding prefix" "long wide charconst" } */

  c = 'ab';		/* { dg-warning "multi-char" "multi-char" } */
  /* Wide charconsts cannot contain more than one wide character.  */
  w = L'ab';		/* { dg-warning "multi-character literal cannot have an encoding prefix" "multi-char wide" } */
}
