/* This testcase failed on Alpha at -O2 when simplifying conditional
   expressions.  */

struct S {
  unsigned long a;
  double b, c;
};

extern double bar (double, double);

int
foo (unsigned long x, unsigned int y, struct S *z)
{
  unsigned int a = z->a;
  int b = y / z->a > 1 ? y / z->a : 1;

  a = y / b < z->a ? y / b : z->a;
  z->c = z->b * bar ((double) a, (double) x);
  return 0;
}
