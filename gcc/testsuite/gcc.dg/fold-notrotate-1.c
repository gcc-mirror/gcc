/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

#define INT_BITS  (sizeof (int) * __CHAR_BIT__)
#define ROL(x, y) ((x) << (y) | (x) >> (INT_BITS - (y)))
#define ROR(x, y) ((x) >> (y) | (x) << (INT_BITS - (y)))

unsigned
rol (unsigned a, unsigned b)
{
  return ~ROL (~a, b);
}

unsigned int
ror (unsigned a, unsigned b)
{
  return ~ROR (~a, b);
}

int
rol_conv1 (int a, unsigned b)
{
  return ~(int)ROL((unsigned)~a, b);
}

int
rol_conv2 (int a, unsigned b)
{
  return ~ROL((unsigned)~a, b);
}

int
rol_conv3 (unsigned a, unsigned b)
{
  return ~(int)ROL(~a, b);
}

#define LONG_BITS  (sizeof (long) * __CHAR_BIT__)
#define ROLL(x, y) ((x) << (y) | (x) >> (LONG_BITS - (y)))
#define RORL(x, y) ((x) >> (y) | (x) << (LONG_BITS - (y)))

unsigned long
roll (unsigned long a, unsigned long b)
{
  return ~ROLL (~a, b);
}

unsigned long
rorl (unsigned long a, unsigned long b)
{
  return ~RORL (~a, b);
}

/* { dg-final { scan-tree-dump-not "~" "optimized" } } */
