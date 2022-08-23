/* { dg-skip-if "missing optab for vectorization" { sparc*-*-* } } */
/* { dg-do assemble } */
/* { dg-additional-options "-O3 -fdump-tree-dce -w" } */

#include <stdint.h>

typedef unsigned int v4si __attribute__ ((vector_size (16)));

__attribute__((noinline, noipa))
void fun(v4si *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (x[i]&(~255)) == 0;
}

/* { dg-final { scan-tree-dump {<=\s*.+\{ 255,.+\}} dce7 { target vect_int } } } */
/* { dg-final { scan-tree-dump-not {&\s*.+\{ 4294967290,.+\}} dce7 { target vect_int } } } */
/* { dg-final { scan-tree-dump-not {\s+bic\s+} dce7 { target { aarch64*-*-* } } } } */
