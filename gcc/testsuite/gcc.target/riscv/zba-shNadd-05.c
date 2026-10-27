/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" } } */

long long
f (int a, int b)
{
  return (a * 3) / b;
}

unsigned int
f1 (unsigned int a, unsigned int b)
{
  return (a * 3) / b;
}

int
f2 (int a, int b)
{
  return (a * 5) % b;
}

unsigned int
f3 (unsigned int a, unsigned int b)
{
  return (a * 9) % b;
}

/* { dg-final { scan-assembler-times "sh1add\t" 2 } } */
/* { dg-final { scan-assembler-times "sh2add\t" 1 } } */
/* { dg-final { scan-assembler-times "sh3add\t" 1 } } */
/* { dg-final { scan-assembler-times "divw\t" 1 } } */
/* { dg-final { scan-assembler-times "divuw\t" 1 } } */
/* { dg-final { scan-assembler-times "remw\t" 1 } } */
/* { dg-final { scan-assembler-times "remuw\t" 1 } } */
