/* { dg-do compile } */
/* { dg-options "-mxsave -mno-avx" } */

#include <immintrin.h>

extern int m;

void
avx_imply_save (void)
{
  _xgetbv (m);
}
