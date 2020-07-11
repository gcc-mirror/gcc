/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

int main (int argc, char *argv [])
{
  vector unsigned char input1 =
    { 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8,
      0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf, 0x11 };
  vector unsigned char input2 =
    { 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8,
      0x9, 0xa, 0xb, 0xc, 0xd, 0x0, 0xf, 0x11 };
  vector unsigned char input3 =
    { 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8,
      0x9, 0xa, 0xb, 0xc, 0xd, 0x0, 0xf, 0x11 };
  vector unsigned char input4 =
    { 0x1, 0x2, 0x0, 0x4, 0x5, 0x6, 0x7, 0x8,
      0x9, 0xa, 0xb, 0xc, 0xd, 0x0, 0xf, 0x11 };

  if (vec_stril_p (input1))
    abort ();
  if (!vec_stril_p (input2))
    abort ();
  if (!vec_stril_p (input3))
    abort ();
  if (!vec_stril_p (input4))
    abort ();

}

/* Enforce that exactly four dot-form instructions which are properly biased
   for the target's endianness implement this built-in.  */

/* { dg-final { scan-assembler-times {\mvstribl\.} 4 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribl\M[^.]} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribr} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribr\.} 4 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstribr\M[^.]} 0 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstribl} 0 { target { le } } } } */
