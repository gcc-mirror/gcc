/* { dg-require-effective-target vect_int } */

#include <stdint.h>
#include "tree-vect.h"

#define N 50
#define TYPE uint16_t 

__attribute__((noipa, noinline, optimize("O1")))
void fun1(TYPE* restrict pixel, TYPE level, int n)
{
  for (int i = 0; i < n; i+=1)
    pixel[i] = (pixel[i] * level) / 0xffffU;
}

__attribute__((noipa, noinline, optimize("O3")))
void fun2(TYPE* restrict pixel, TYPE level, int n)
{
  for (int i = 0; i < n; i+=1)
    pixel[i] = (pixel[i] * level) / 0xffffU;
}

#include "vect-div-bitmask.h"

/* { dg-final { scan-tree-dump-not "vect_recog_divmod_pattern: detected" "vect" { target aarch64*-*-* } } } */
