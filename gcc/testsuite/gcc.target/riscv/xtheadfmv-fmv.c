/* { dg-do compile { target { rv32 } } } */
/* { dg-options "-march=rv32gc_xtheadfmv -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

double
ll2d (long long ll)
{
  return *(double*)&ll;
}

long long
d2ll (double d)
{
  return *(long long*)&d;
}

/* { dg-final { scan-assembler {\mfmv\.w.x\M} } } */
/* { dg-final { scan-assembler "th.fmv.hw.x" } } */
/* { dg-final { scan-assembler {\mfmv\.x.w\M} } } */
/* { dg-final { scan-assembler "th.fmv.x.hw" } } */
/* { dg-final { scan-assembler-not "\tsw\t" } } */
/* { dg-final { scan-assembler-not "\tfld\t" } } */
/* { dg-final { scan-assembler-not "\tfsd\t" } } */
/* { dg-final { scan-assembler-not "\tlw\t" } } */
