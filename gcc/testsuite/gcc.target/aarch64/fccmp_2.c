/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+nofp16" } */

int
hf_and (_Float16 a, _Float16 b, _Float16 c, _Float16 d)
{
  return (a == b) && (c == d);
}

/* FEAT_FP16 is required for half precision compare instructions.  */
/* { dg-final { scan-assembler-not {\tfcmpe?\th} } } */
/* { dg-final { scan-assembler-not {\tfccmpe?\th} } } */
