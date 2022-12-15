/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fno-vect-cost-model" { target aarch64*-*-* } } */

#include <stdint.h>
#include "tree-vect.h"

#define N 50
#define TYPE uint32_t 

__attribute__((noipa, noinline, optimize("O1")))
void fun1(TYPE* restrict pixel, TYPE level, int n)
{
  for (int i = 0; i < n; i+=1)
    pixel[i] = (pixel[i] * (uint64_t)level) / 0xffffffffUL;
}

__attribute__((noipa, noinline, optimize("O3")))
void fun2(TYPE* restrict pixel, TYPE level, int n)
{
  for (int i = 0; i < n; i+=1)
    pixel[i] = (pixel[i] * (uint64_t)level) / 0xffffffffUL;
}

#include "vect-div-bitmask.h"

/* { dg-final { scan-tree-dump-not "vect_recog_divmod_pattern: detected" "vect" { target aarch64*-*-* } } } */
