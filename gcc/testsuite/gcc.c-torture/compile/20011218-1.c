/* This testcase failed on Alpha at -O2 because $27 hard register
   for the indirect call was exposed too early and reload couldn't
   allocate it for multiplication and division.  */

struct S {
  int a, b;
  void (*f) (long, int);
};

void foo (struct S *x)
{
  long c = x->a * 50;
  c /= (long) x->b;
  c *= (long) x->b;
  x->f (c, 0);
}
