/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef int (*funct)(int, int, int);

extern int f(int, int, int);
extern int g(int, int, int);
extern int h(int, funct, funct);

static int baz(int x, int y, int z)
{
  return x + y + z;
}

static int bar(int n, funct f1, funct f2)
{
  return h(n, f1, f2) + f1(0, 1, 2);
}

static int foo(int n, funct f1, funct f2)
{
  return bar(n, f1, f2) + f2(0, 1, 2);
}

int main(void)
{
  return foo(0, f, g)
#ifndef ICE2
   + foo(0, baz, g)
#endif
  ;
}
