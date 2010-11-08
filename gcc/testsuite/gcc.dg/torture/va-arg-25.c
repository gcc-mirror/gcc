/* Varargs and vectors!  */

/* { dg-do run } */
/* { dg-options "-msse" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target sse_runtime { target { i?86-*-* x86_64-*-* } } } */

#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>

#define vector __attribute__((vector_size(16)))

const vector unsigned int v1 = {10,11,12,13};
const vector unsigned int v2 = {20,21,22,23};

void foo(int a, ...)
{
  va_list args;
  vector unsigned int v;

  va_start (args, a);
  v = va_arg (args, vector unsigned int);
  if (a != 1 || memcmp (&v, &v1, sizeof (v)) != 0)
    abort ();
  a = va_arg (args, int);
  if (a != 2)
    abort ();
  v = va_arg (args, vector unsigned int);
  if (memcmp (&v, &v2, sizeof (v)) != 0)
    abort ();
  va_end (args);
}

int main(void)
{
#if INT_MAX == 2147483647
  foo (1, (vector unsigned int){10,11,12,13}, 2,
       (vector unsigned int){20,21,22,23});
#endif
  return 0;
}

