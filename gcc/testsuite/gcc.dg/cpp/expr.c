/* Copyright (C) 2000, 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Test we get signedness of ?: operator correct.  We would skip
   evaluation of one argument, and might therefore not transfer its
   unsignedness to the result.  */

/* Neil Booth, 19 Jul 2002.  */

#if (1 ? -2: 0 + 1U) < 0
#error				/* { dg-bogus "error" } */
#endif

#if (0 ? 0 + 1U: -2) < 0
#error				/* { dg-bogus "error" } */
#endif
