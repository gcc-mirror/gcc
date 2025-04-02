/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long long sub (unsigned long long a, unsigned long long b)
{
  b = (b << 50) >> 49;
  unsigned int x = a + b;
  return x;
}

/* { dg-final { scan-assembler-not {\msh1add} } } */
