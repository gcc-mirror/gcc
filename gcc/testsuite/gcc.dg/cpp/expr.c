/* Copyright (C) 2000, 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-additional-options "-Wall" } */

/* Test we get signedness of ?: operator correct.  We would skip
   evaluation of one argument, and might therefore not transfer its
   unsignedness to the result.  */

/* Neil Booth, 19 Jul 2002.  */

#if (1 ? -2: 0 + 1U) < 0 /* { dg-warning {the left operand of ":" changes sign} } */
#error				/* { dg-bogus "error" } */
#endif

#if (0 ? 0 + 1U: -2) < 0 /* { dg-warning {the right operand of ":" changes sign} } */
#error				/* { dg-bogus "error" } */
#endif

/* PR preprocessor/112701 */
#if (0 ? 0/0u : -1) < 0 /* { dg-warning {the right operand of ":" changes sign} } */
#error /* { dg-bogus "error" } */
#endif

#if (0 ? 0u/0 : -1) < 0 /* { dg-warning {the right operand of ":" changes sign} } */
#error /* { dg-bogus "error" } */
#endif

#if (1 ? -1 : 0/0u) < 0 /* { dg-warning {the left operand of ":" changes sign} } */
#error /* { dg-bogus "error" } */
#endif

#if (1 ? -1 : 0u/0) < 0 /* { dg-warning {the left operand of ":" changes sign} } */
#error /* { dg-bogus "error" } */
#endif
