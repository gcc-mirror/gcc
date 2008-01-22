/* PR c++/34859.
   It is ok to redefine __STDC_CONSTANT_MACROS and __STDC_LIMIT_MACROS.  */

/* { dg-do preprocess } */

#define __STDC_CONSTANT_MACROS 1
#define __STDC_CONSTANT_MACROS 1

#define __STDC_LIMIT_MACROS 1
#define __STDC_LIMIT_MACROS 1
