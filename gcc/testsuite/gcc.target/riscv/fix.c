/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* }  { "-O0" } } */

int
foo (double n)
{
  return n;
}

int
foo_1 (float n)
{
  return n;
}

unsigned int
foo_2 (double n)
{
  return n;
}

unsigned int
foo_3 (float n)
{
  return n;
}

/* { dg-final { scan-assembler-times {\mfcvt.w.d} 1 } } */
/* { dg-final { scan-assembler-times {\mfcvt.w.s} 1 } } */
/* { dg-final { scan-assembler-times {\mfcvt.wu.d} 1 } } */
/* { dg-final { scan-assembler-times {\mfcvt.wu.s} 1 } } */
/* { dg-final { scan-assembler-not "\\ssext.w\\s" } } */

