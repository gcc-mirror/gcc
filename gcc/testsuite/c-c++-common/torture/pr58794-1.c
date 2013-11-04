/* { dg-do compile } */

struct S0
{
  int f;
};

struct S1
{
  struct S0 f1;
  volatile int f2;
};

struct S2
{
  struct S1 g;
} a, b; 

static int *c[1][2] = {{0, (int *)&a.g.f2}};
static int d; 

int
main ()
{
  for (d = 0; d < 1; d++)
    for (b.g.f1.f = 0; b.g.f1.f < 1; b.g.f1.f++)
      *c[b.g.f1.f][d + 1] = 0;
  return 0;
}
