/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* Test various combinations of missing parentheses give the correct
   missing parenthesis message.  */

/* { dg-do preprocess } */
/* { dg-options "-fshow-column" } */
#if (1          /* { dg-error "5:missing '\\)'" "missing ')' no. 1" } */
#endif

#if 2 * (3 + 4	/* { dg-error "9:missing '\\)'" "missing ')' no. 2" } */
#endif

#if (2))	/* { dg-error "8:missing '\\('" "missing '(' no. 1" } */
#endif

#if )		/* { dg-error "5:missing '\\('" "missing '(' no. 2" } */
#endif

#if 4)		/* { dg-error "6:missing '\\('" "missing '(' no. 3" } */
#endif

#if (		/* { dg-error "5:missing '\\)'" "missing ')' no. 3" } */
#endif

#if ((2 + 3) + 5 /* { dg-error "5:missing '\\)'" "missing ')' no. 3" } */
#endif

#if ((2 + 3 + 5 /* { dg-error "6:missing '\\)'" "missing ')' no. 3" } */
#endif
