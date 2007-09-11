/* Copyright (c) 2002 Free Software Foundation Inc.  */

/* Test that macros are not expanded in the <> quotes of #inlcude.  */

/* vxWorksCommon.h uses the "#" operator to construct the name of an
   include file, thus making the file incompatible with -traditional-cpp.
   Newlib uses ## when including stdlib.h as of 2007-09-07.  */
/* { dg-do preprocess { target { { ! vxworks_kernel } && { ! newlib } } } } */

#define __STDC__ 1		/* Stop complaints about non-ISO compilers.  */
#define stdlib 1
#include <stdlib.h>		/* { dg-bogus "o such file or directory" } */
