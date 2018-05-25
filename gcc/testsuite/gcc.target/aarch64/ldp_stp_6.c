/* { dg-options "-O2" } */

typedef float __attribute__ ((vector_size (8))) vec;

struct pair
{
  vec e1;
  double e2;
};

vec tmp;

void
stp (struct pair *p)
{
  p->e1 = tmp;
  p->e2 = 1.0;

  /* { dg-final { scan-assembler "stp\td\[0-9\]+, d\[0-9\]+, \\\[x\[0-9\]+\\\]" } } */
}
