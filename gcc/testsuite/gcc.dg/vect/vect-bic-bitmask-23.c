/* { dg-skip-if "missing optab for vectorization" { sparc*-*-* } } */
/* { dg-do assemble } */
/* { dg-additional-options "-O1 -fdump-tree-dce -w" } */

#include <stdint.h>

typedef unsigned int v4si __attribute__ ((vector_size (16)));

__attribute__((noinline, noipa))
v4si fun(v4si x)
{
    v4si mask = { 255, 15, 1, 0xFFFF };
    v4si zeros = {0};
    return (x & ~mask) == zeros;
}

/* { dg-final { scan-tree-dump {<=\s*.+\{ 255, 15, 1, 65535 \}} dce7 { target vect_int } } } */
