/* { dg-do compile } */
/* { dg-options "-O2 -mshstk" } */
/* { dg-final { scan-assembler-times "setssbsy" 1 } } */

#include <immintrin.h>

void f2 (void)
{
  _setssbsy ();
}
