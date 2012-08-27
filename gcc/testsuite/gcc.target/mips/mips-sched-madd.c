/* Test for case where another independent multiply insn may interfere
   with a macc chain.  */
/* { dg-do compile } */
/* { dg-options "-march=24kf" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

NOMIPS16 int foo (int a, int b, int c, int d, int e, int f, int g)
{
  int temp;
  int acc;

  acc = a * b;
  temp = a * c;
  acc = d * e + acc;
  acc = f * g + acc;
  return acc > temp ? acc : temp;
}

/* { dg-final { scan-assembler "\tmult\t" } } */
/* { dg-final { scan-assembler "\tmadd\t" } } */
