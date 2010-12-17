/* PR tree-optimization/45903 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long long a, b;
unsigned char c;

void
f1 (void)
{
  c = (a >> 8) + (b >> 8);
}

void
f2 (void)
{
  c = (a >> 8) | (b >> 8);
}

void
f3 (void)
{
  c = (a >> 16) ^ (b >> 56);
}

unsigned char
f4 (void)
{
  return (a >> 48) + (b >> 40);
}

unsigned char
f5 (void)
{
  return (a >> 32) | (b >> 16);
}

unsigned char
f6 (void)
{
  return (a >> 24) ^ (b >> 32);
}

/* { dg-final { scan-assembler-not "shr\[qdl\]" } } */
