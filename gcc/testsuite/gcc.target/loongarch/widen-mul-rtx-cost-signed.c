/* Verify optimization for mulw.d.w,
   which can help with the replacement of the high-latency div.w.  */
/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-O3" } */

int
test (int a)
{
  return a / 3;
}

/* { dg-final { scan-assembler {\tmulw.d.w\t} } } */
/* { dg-final { scan-assembler-not {\tdiv.w\t} } } */
