/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-std=c99" } */

/* Source: Neil Booth, 6 Aug 2002.

   Tests that we DTRT with varargs commas for a single-parameter macro
   when in standards-conforming mode.  */

#define f(...) , ## __VA_ARGS__

/* The comma from f's expansion should be retained (standards
   conforming mode only).  Tests that it isn't in non-standards mode
   include macro8.c and vararg1.c.  */
#if 2 f() 3		  /* { dg-bogus "missing binary operator" } */
#endif
