struct X { int i; int j; };
#define FOO struct X
#define FOO10(x) FOO x ## 0; FOO x ## 1; FOO x ## 2; FOO x ## 3; FOO x ## 4; FOO x ## 5; FOO x ## 6; FOO x ## 7; FOO x ## 8; FOO x ## 9;
#define FOO100(x) FOO10(x ## 0) FOO10(x ## 1) FOO10(x ## 2) FOO10(x ## 3) FOO10(x ## 4) FOO10(x ## 5) FOO10(x ## 6) FOO10(x ## 7) FOO10(x ## 8) FOO10(x ## 9)
  FOO100(x0)
  FOO100(x1)
  FOO100(x2)
  FOO100(x3)
  FOO100(x4)
  FOO100(x5)
  FOO100(x6)
  FOO100(x7)
  FOO100(x8)
  FOO100(x9)

#define COO(n,f) case n: p = &f; break;
#define COO10(n,f) COO(n ## 0, f ## 0) COO(n ## 1, f ## 1) COO(n ## 2, f ## 2) COO(n ## 3, f ## 3) COO(n ## 4, f ## 4) COO(n ## 5, f ## 5) COO(n ## 6, f ## 6) COO(n ## 7, f ## 7) COO(n ## 8, f ## 8) COO(n ## 9, f ## 9)
#define COO100(n,f) COO10(n ## 0, f ## 0) COO10(n ## 1, f ## 1) COO10(n ## 2, f ## 2) COO10(n ## 3, f ## 3) COO10(n ## 4, f ## 4) COO10(n ## 5, f ## 5) COO10(n ## 6, f ## 6) COO10(n ## 7, f ## 7) COO10(n ## 8, f ## 8) COO10(n ## 9, f ## 9)

int foo(int i)
{
  struct X *p = 0;
  x000.i = 0;
  x599.j = 0;
  switch (i)
    {
  COO100(1, x0)
  COO100(2, x1)
  COO100(3, x2)
  COO100(4, x3)
  COO100(5, x4)
  COO100(6, x5)
  COO100(7, x6)
  COO100(8, x7)
  COO100(9, x8)
  COO100(10, x9)
    }
  return p->j;
}
