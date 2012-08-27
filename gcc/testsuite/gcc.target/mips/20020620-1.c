/* { dg-do compile } */
/* { dg-options "-mlong64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
int foo (int *x, int i)
{
  return x[i] + i;
}
/* { dg-final { scan-assembler-not "\tmove" } } */
