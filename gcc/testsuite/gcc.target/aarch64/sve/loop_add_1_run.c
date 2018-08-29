/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3" } */

#include "loop_add_1.c"

#define ELEMS 10

int __attribute__ ((optimize (1)))
main (void)
{
  int in1[ELEMS] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  int in2[ELEMS] = { 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
  int out[ELEMS];
  int check[ELEMS] = { 3, 5, 7, 9, 11, 13, 15, 17, 19, 21 };

  vadd (out, in1, in2, ELEMS);

  for (int i = 0; i < ELEMS; ++i)
    if (out[i] != check[i])
      __builtin_abort ();

  return 0;
}
