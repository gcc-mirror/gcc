/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* No abi attribute should be emitted when nothing relevant happened.  */
/* { dg-final { scan-assembler-not "gnu_attribute" } } */

#include <stdarg.h>

/* Local use is ok.  */

typedef int v4si __attribute__((vector_size(16)));

static
v4si __attribute__((__noinline__))
foo (v4si a)
{
  return a + (v4si){ 1, 2, 3, 4 };
}

int
bar (int a)
{
  return foo ((v4si){ 1, 1, 1, 1 })[1];
}

/* Big vector type only used as function argument and return value
   without being a struct/union member.  The alignment change is not
   relevant here.  */

typedef double v4df __attribute__((vector_size(32)));

v4df
add (v4df a, v4df b, v4df c, v4df d,
     v4df e, v4df f, v4df g, v4df h, v4df i)
{
  return a + b + c + d + e + f + g + h + i;
}

double
bar2 (int n, ...)
{
  double ret;
  v4df a;
  va_list va;

  va_start (va, n);
  ret = va_arg (va, v4df)[2];
  va_end (va);

  return ret;
}
