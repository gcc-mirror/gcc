/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

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

/* { dg-final { scan-assembler-times {\mvstrihl\.} 4 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl\M[^.]} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr\.} 4 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr\M[^.]} 0 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl} 0 { target { le } } } } */
