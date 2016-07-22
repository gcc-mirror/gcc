/* Test the absence of a spurious move from x8 to x0 for functions
   return structures.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct s
{
  long x;
  long y;
  long z;
};

struct s __attribute__((noinline))
foo (long a, long d, long c)
{
  struct s b;
  b.x = a;
  b.y = d;
  b.z = c;
  return b;
}

int
main (void)
{
  struct s x;
  x = foo ( 10, 20, 30);
  return x.x + x.y + x.z;
}

/* { dg-final { scan-assembler-not "mov\tx0, x8" } } */
