/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options -std=gnu99 } */

/* Source: Neil Booth, 6 Aug 2002.

   Tests that we DTRT with varargs commas.  */

#define g(a, ...) a , ## __VA_ARGS__

/* The comma from g's expansion should be retained.  */
#if g (2, ) 3		  /* { dg-bogus "missing binary operator" } */
#endif
