/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess { target i?86-*-* } } */

/* Tests that traditional numbers are signed, unless otherwise
   specified.  This test assumes a 32 bit target.

   Neil Booth, 5 Aug 2001.  Inspired by PR 3824.  */

#if 0xffffffffffffffff >= 0
# error	0xffffffffffffffff /* { dg-bogus "0xffffffffffffffff" "0xffffffffffffffff positive" } */
#endif

#if 0xffffffffffffffffU <= 0
# error	0xffffffffffffffffU	/* { dg-bogus "0xffffffffffffffffU" "0xffffffffffffffffU negative" } */
#endif
