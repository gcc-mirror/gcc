/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-std=c99" } */

/* This tests universal character sequences.

   Neil Booth, 22 May 2001.
   Richard Henderson, 3 Apr 2002.  */

#include <limits.h>

#define unsigned        +0
#define int             +0 
#define char            +1
#define short           +2 
#define long            +3

#if __WCHAR_TYPE__ == 0
# define WCHAR_MAX      INT_MAX
#elif __WCHAR_TYPE__ == 1
# define WCHAR_MAX      CHAR_MAX
#elif __WCHAR_TYPE__ == 2
# define WCHAR_MAX      SHRT_MAX
#elif __WCHAR_TYPE__ == 3
# define WCHAR_MAX      LONG_MAX
#else
# error wacky wchar_t
#endif

#undef unsigned
#undef int
#undef char
#undef short
#undef long

#if L'\u1234' != 0x1234
#error bad short ucs	/* { dg-bogus "bad" "bad u1234 evaluation" } */
#endif

#if WCHAR_MAX >= 0x7ffffff
# if L'\U1234abcd' != 0x1234abcd
#  error bad long ucs	/* { dg-bogus "bad" "bad U1234abcd evaluation" } */
# endif
#endif

void foo ()
{
  int c;

  c = L'\ubad';		/* { dg-error "incomplete" "incomplete UCN 1" } */
  c = L"\U1234"[0];	/* { dg-error "incomplete" "incompete UCN 2" } */

  c = L'\u000x';	/* { dg-error "incomplete" "non-hex digit in UCN" } */
  /* If sizeof(HOST_WIDE_INT) > sizeof(wchar_t), we can get a multi-character
     constant warning even for wide characters.  */
  /* { dg-warning "too long|multi-character" "" { target *-*-* } 54 } */

  c = '\u0024';		/* { dg-bogus "invalid" "0024 is a valid UCN" } */
  c = "\u0040"[0];	/* { dg-bogus "invalid" "0040 is a valid UCN" } */
  c = L'\u00a0';	/* { dg-bogus "invalid" "00a0 is a valid UCN" } */
  c = '\U00000060';	/* { dg-bogus "invalid" "0060 is a valid UCN" } */

  c = '\u0025';		/* { dg-error "not a valid" "0025 invalid UCN" } */
  c = L"\uD800"[0];	/* { dg-error "not a valid" "D800 invalid UCN" } */
  c = L'\U0000DFFF';	/* { dg-error "not a valid" "DFFF invalid UCN" } */
}
