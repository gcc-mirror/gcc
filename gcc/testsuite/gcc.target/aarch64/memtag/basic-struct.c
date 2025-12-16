/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

struct A
{
  long a;
  long b;
  long c;
  long d;
};

extern void use (struct A *a);

long f (void)
{
  struct A a = {0, 0, 64, (long) &a};
  use (&a);
  return a.b;
}

/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst2g\t} 2 } } */
