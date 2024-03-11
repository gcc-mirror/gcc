/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long long sub1(unsigned long long a, unsigned long long b)
{
  b = (b << 32) >> 31;
  unsigned int x = a + b;
  return x;
}

long long sub2(unsigned long long a, unsigned long long b)
{
  return (unsigned int)(a + (b << 1));
}

long long sub3(unsigned long long a, unsigned long long b)
{
  return (a + (b << 1)) & ~0u;
}

/* { dg-final { scan-assembler-times {\msh1add} 3 } } */
/* { dg-final { scan-assembler-times "zext.w\t" 3 } } */
