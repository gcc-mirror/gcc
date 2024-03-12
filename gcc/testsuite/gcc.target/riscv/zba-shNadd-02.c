/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zba -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

long test_1(long a, long b)
{
  return a + (b << 1);
}
long test_2(long a, long b)
{
  return a + (b << 2);
}
long test_3(long a, long b)
{
  return a + (b << 3);
}

/* { dg-final { scan-assembler-times {\msh1add} 1 } } */
/* { dg-final { scan-assembler-times {\msh2add} 1 } } */
/* { dg-final { scan-assembler-times {\msh3add} 1 } } */
