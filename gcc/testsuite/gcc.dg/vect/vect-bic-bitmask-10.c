/* { dg-skip-if "missing optab for vectorization" { sparc*-*-* } } */
/* { dg-do run } */
/* { dg-additional-options "-O3 -save-temps -fdump-tree-dce -w" } */

#include <stdint.h>

__attribute__((noinline, noipa))
void fun1(int32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (x[i]&(~255)) == 0;
}

__attribute__((noinline, noipa, optimize("O1")))
void fun2(int32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (x[i]&(~255)) == 0;
}

#define TYPE int32_t
#include "../bic-bitmask.h"

/* { dg-final { scan-tree-dump {<=\s*.+\{ 255,.+\}} dce7 { target vect_int } } } */
/* { dg-final { scan-tree-dump-not {&\s*.+\{ 4294967290,.+\}} dce7 { target vect_int } } } */
/* { dg-final { scan-tree-dump-not {\s+bic\s+} dce7 { target { aarch64*-*-* } } } } */

