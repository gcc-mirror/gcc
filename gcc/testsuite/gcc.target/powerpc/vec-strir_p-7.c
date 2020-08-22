/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string isolate right-justified on array of signed short.  */
int
sirj_p (vector signed short arg)
{
  return vec_strir_p (arg);
}

int main (int argc, short *argv [])
{
  vector signed short input1 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector signed short input2 =
    { 0x1, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector signed short input3 =
    { 0x1, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 };
  vector signed short input4 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 };

  if (sirj_p (input1))
    abort ();
  if (!sirj_p (input2))
    abort ();
  if (!sirj_p (input3))
    abort ();
  if (!sirj_p (input4))
    abort ();

}
