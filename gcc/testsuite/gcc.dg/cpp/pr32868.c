/* PR preprocessor/32868.  It is ok to redefine __STDC_FORMAT_MACROS.  */

/* { dg-do preprocess } */

#define __STDC_FORMAT_MACROS 1
#define __STDC_FORMAT_MACROS 1
