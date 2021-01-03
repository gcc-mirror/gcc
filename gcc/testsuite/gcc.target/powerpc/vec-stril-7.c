/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string isolate left-justified on array of signed short.  */
vector signed short
silj (vector signed short arg)
{
  return vec_stril (arg);
}

int main (int argc, short *argv [])
{
  vector signed short input1 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector signed short expected1 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector signed short input2 =
    { 0x1, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector signed short expected2 =
    { 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
  vector signed short input3 =
    { 0x1, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 };
  vector signed short expected3 =
    { 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
  vector signed short input4 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 };
  vector signed short expected4 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 };

  if (!vec_all_eq (silj (input1), expected1))
    abort ();
  if (!vec_all_eq (silj (input2), expected2))
    abort ();
  if (!vec_all_eq (silj (input3), expected3))
    abort ();
  if (!vec_all_eq (silj (input4), expected4))
    abort ();
}
