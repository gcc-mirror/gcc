/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess { target i?86-*-* } } */

/* { dg-options "-traditional" } */

/* Tests that traditional numbers are signed, unless otherwise
   specified.  This test assumes a 32 bit target.

   Neil Booth, 5 Aug 2001.  Inspired by PR 3824.  */

#if 0xffffffff >= 0
# error	0xffffffff	/* { dg-bogus "0xffffffff" "0xffffffff positive" } */
#endif

#if 0xffffffffU <= 0
# error	0xffffffffU	/* { dg-bogus "0xffffffffU" "0xffffffffU negative" } */
#endif
