/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler "gnu_attribute 8, 2" } } */

#include <stdarg.h>

typedef int __attribute__((vector_size(16))) v4si;

int
bar (int n, ...)
{
  int ret;
  v4si a;
  va_list va;

  va_start (va, n);
  ret = va_arg (va, v4si)[2];
  va_end (va);

  return ret;
}
