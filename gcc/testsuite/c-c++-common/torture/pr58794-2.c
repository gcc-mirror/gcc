/* { dg-do compile } */

struct S 
{
  volatile int f;
} a;

unsigned int b;

static int *c[1][2] = {{0, (int *)&a.f}};
static unsigned int d;

int 
main ()
{
  for (; d < 1; d++)
    for (; b < 1; b++)
      *c[b][d + 1] = 0;

  return 0;
}
