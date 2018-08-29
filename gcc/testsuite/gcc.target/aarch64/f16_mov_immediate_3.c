/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC target "+nofp16"

__fp16 f4 ()
{
  __fp16 a = 0.1;

  __fp16 z = a * a;
  return z;
}

/* { dg-final { scan-assembler-times {dup\tv[0-9]+.4h, w[0-9]+} 1 } } */
