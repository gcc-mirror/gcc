/* { dg-do compile } */
/* { dg-options "-march=vr4130 -mfix-vr4130" } */
NOMIPS16 unsigned int
foo (unsigned int x, unsigned int y)
{
  return x % y;
}
/* { dg-final { scan-assembler "\tmacchi\t" } } */
