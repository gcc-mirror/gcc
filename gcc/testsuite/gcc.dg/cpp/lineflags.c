/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-fno-show-column" } */

/* This tests whether various combinations of flags are correctly
   accepted after #line.

   Neil Booth, 8 Dec 2000.  */

#line 12 "file" 3		/* { dg-warning "extra tokens" } */

# 14 "file" 1 1			/* { dg-error "invalid flag" } */
# 15 "file" 1 2			/* { dg-error "invalid flag" } */
# 16 "file" 1 3 3		/* { dg-error "invalid flag" } */
# 17 "file" 1 4			/* { dg-error "invalid flag" } */
# 18 "file" 2 3 4 4		/* { dg-error "invalid flag" } */
# 19 "file" 2 4			/* { dg-error "invalid flag" } */
# 20 "file" 2 2			/* { dg-error "invalid flag" } */
# 21 "file" 2 1			/* { dg-error "invalid flag" } */
# 22 "file" 4			/* { dg-error "invalid flag" } */
# 23 "file" 4 5			/* { dg-error "invalid flag" } */
# 24 "file" 0			/* { dg-error "invalid flag" } */
# 25 "file" 5			/* { dg-error "invalid flag" } */
# 26 "file" foo			/* { dg-error "invalid flag" } */


# 29 "file" 1			/* { dg-bogus "invalid flag" } */
# 30 "file" 2			/* { dg-bogus "invalid flag" } */
# 31 "file" 1 3			/* { dg-bogus "invalid flag" } */
# 32 "file" 2 3			/* { dg-bogus "invalid flag" } */
# 33 "file" 1 3 4		/* { dg-bogus "invalid flag" } */
# 34 "file" 2 3	4		/* { dg-bogus "invalid flag" } */
# 35 "file" 3			/* { dg-bogus "invalid flag" } */
# 36 "file" 3 4			/* { dg-bogus "invalid flag" } */
