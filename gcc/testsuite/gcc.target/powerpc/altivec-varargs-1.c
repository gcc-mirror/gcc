/* { dg-do run { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mabi=altivec -fno-inline" } */

#include <stdarg.h>
#include <signal.h>

#include "altivec_check.h"

#define vector __attribute__((vector_size (16)))

const vector unsigned int v1 = {10,11,12,13};
const vector unsigned int v2 = {20,21,22,23};
const vector unsigned int v3 = {30,31,32,33};
const vector unsigned int v4 = {40,41,42,43};

void foo(vector unsigned int a, ...)
{
  va_list args;
  vector unsigned int v;

  va_start (args, a);
  if (memcmp (&a, &v1, sizeof (v)) != 0)
    abort ();
  v = va_arg (args, vector unsigned int);
  if (memcmp (&v, &v2, sizeof (v)) != 0)
    abort ();
  v = va_arg (args, vector unsigned int);
  if (memcmp (&v, &v3, sizeof (v)) != 0)
    abort ();
  v = va_arg (args, vector unsigned int);
  if (memcmp (&v, &v4, sizeof (v)) != 0)
    abort ();
  va_end (args);
}

void bar(vector unsigned int a, ...)
{
  va_list args;
  vector unsigned int v;
  int b;

  va_start (args, a);
  if (memcmp (&a, &v1, sizeof (v)) != 0)
    abort ();
  b = va_arg (args, int);
  if (b != 2)
    abort ();
  v = va_arg (args, vector unsigned int);
  if (memcmp (&v, &v2, sizeof (v)) != 0)
    abort ();
  v = va_arg (args, vector unsigned int);
  if (memcmp (&v, &v3, sizeof (v)) != 0)
    abort ();
  va_end (args);
}


int main1(void)
{
  /* In this call, in the Darwin ABI, the first argument goes into v2
     the second one into r9-r10 and memory,
     and the next two in memory.  */
  foo ((vector unsigned int){10,11,12,13},
       (vector unsigned int){20,21,22,23},
       (vector unsigned int){30,31,32,33},
       (vector unsigned int){40,41,42,43});
  /* In this call, in the Darwin ABI, the first argument goes into v2
     the second one into r9, then r10 is reserved and
     there are two words of padding in memory, and the next two arguments
     go after the padding.  */
  bar ((vector unsigned int){10,11,12,13}, 2,
       (vector unsigned int){20,21,22,23},
       (vector unsigned int){30,31,32,33});
  return 0;
}

int main (void)
{
  /* Exit on systems without AltiVec.  */
  altivec_check ();

  return main1 ();
}
