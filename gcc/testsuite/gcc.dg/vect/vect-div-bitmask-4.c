/* { dg-require-effective-target vect_int } */

#include <stdint.h>
#include "tree-vect.h"

typedef unsigned __attribute__((__vector_size__ (16))) V;

static __attribute__((__noinline__)) __attribute__((__noclone__)) V
foo (V v, unsigned short i)
{
  v /= i;
  return v;
}

int
main (void)
{
  V v = foo ((V) { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff }, 0xffff);
#pragma GCC novector
  for (unsigned i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    if (v[i] != 0x00010001)
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "vect_recog_divmod_pattern: detected" "vect" { target aarch64*-*-* } } } */
