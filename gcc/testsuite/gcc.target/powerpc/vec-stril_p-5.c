/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string isolate left-justified on array of unsigned short.  */
int
silj_p (vector unsigned short arg)
{
  return vec_stril_p (arg);
}

int main (int argc, short *argv [])
{
  vector unsigned short input1 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector unsigned short input2 =
    { 0x1, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector unsigned short input3 =
    { 0x1, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 };
  vector unsigned short input4 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 };

  if (silj_p (input1))
    abort ();
  if (!silj_p (input2))
    abort ();
  if (!silj_p (input3))
    abort ();
  if (!silj_p (input4))
    abort ();
}
