/* Copyright (c) 2002 Free Software Foundation Inc.  */

/* Test that macros are not expanded in the <> quotes of #inlcude.  */

/* { dg-do preprocess } */

#define __STDC__ 1	/* Stop complaints about non-ISO compilers.  */
#define stdio 1
#include <stdio.h>		/* { dg-bogus "o such file or directory" } */
