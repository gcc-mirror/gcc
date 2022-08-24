/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (int x, int y)
{
  return (x == 21) && (y == 69);
}

/* { dg-final { scan-assembler-not "cvt.u16.u8" } } */
/* { dg-final { scan-assembler-not "cvt.u32.u16" } } */
/* { dg-final { scan-assembler-not "cvt.u32.u8" } } */

/* { dg-final { scan-assembler-times "setp.eq.u32" 2 } } */
/* { dg-final { scan-assembler-times "selp.u32" 1 } } */
/* { dg-final { scan-assembler-times "and.pred" 1 } } */
