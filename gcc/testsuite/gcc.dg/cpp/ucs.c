/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-std=c99" } */

/* This tests universal character sequences.

   Neil Booth, 22 May 2001.  */

#if L'\u1234' != 0x1234
#error bad short ucs	/* { dg-bogus "bad" "bad \u1234 evaluation" } */
#endif
#if L'\U1234abcd' != 0x1234abcd
#error bad long ucs	/* { dg-bogus "bad" "bad \U1234abcd evaluation" } */
#endif

void foo ()
{
  int c;

  c = L'\ubad';		/* { dg-error "incomplete" "incompete UCN 1" } */
  c = L"\U1234"[0];	/* { dg-error "incomplete" "incompete UCN 2" } */

  c = L'\u000x';	/* { dg-error "non-hex" "non-hex digit in UCN" } */
			/* { dg-warning "too long" "" { target *-*-* } 24 } */

  c = '\u0024';		/* { dg-bogus "invalid" "0024 is a valid UCN" } */
  c = "\u0040"[0];	/* { dg-bogus "invalid" "0040 is a valid UCN" } */
  c = '\u00a0';		/* { dg-bogus "invalid" "00a0 is a valid UCN" } */
  c = '\U00000060';	/* { dg-bogus "invalid" "0060 is a valid UCN" } */

  c = '\u0025';		/* { dg-error "range" "0025 is an invalid UCN" } */
  c = L"\uD800"[0];	/* { dg-error "range" "D800 is an invalid UCN" } */
  c = L'\U0000DFFF';	/* { dg-error "range" "DFFF is an invalid UCN" } */
}
