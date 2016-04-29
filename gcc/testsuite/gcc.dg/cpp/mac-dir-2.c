/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* Source: Neil Booth, 26 Feb 2002.

   Test that we allow directives in macro arguments.  */

/* { dg-do preprocess } */

#define f(x) x

f (
#if 1		/* { dg-error "not portable" } */
1)
#endif
