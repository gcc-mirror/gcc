/* { dg-do compile } */
/* { dg-options "-O1" } */

int neq (int a, int b)
{
  return a != b;
}

int eq (int a, int b)
{
  return a == b;
}

int lt (unsigned int a, unsigned int b)
{
  return a < b;
}

int leq (unsigned int a, unsigned int b)
{
  return a <= b;
}

int geq (unsigned int a, unsigned int b)
{
  return a >= b;
}

int gt (unsigned int a, unsigned int b)
{
  return a > b;
}

/* { dg-final { scan-assembler-times "xor\t%" 2 } } */
/* { dg-final { scan-assembler-times "addx\t%" 3 } } */
/* { dg-final { scan-assembler-times "subx\t%" 3 } } */
/* { dg-final { scan-assembler-times "cmp\t%" 6 } } */
/* { dg-final { scan-assembler-not "sra\t%" { target lp64 } } } */
