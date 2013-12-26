/* { dg-do preprocess } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-march=i686" } */

#ifndef __tune_i686__
#error "__tune_i686__ should be defined for this test"
#endif
