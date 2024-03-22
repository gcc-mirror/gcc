/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fvect-cost-model=dynamic" } */

void bar (int *);
int foo (int *p, int a, int b)
{
  int x[4];
  int tem0, tem1, tem2, tem3;
  int sum = 0;
  p = __builtin_assume_aligned (p, __BIGGEST_ALIGNMENT__);
  tem0 = p[0] + 1 + a;
  sum += tem0;
  x[0] = tem0;
  tem1 = p[1] + 2 + b;
  sum += tem1;
  x[1] = tem1;
  tem2 = p[2] + 3 + b;
  sum += tem2;
  x[2] = tem2;
  tem3 = p[3] + 4 + a;
  sum += tem3;
  x[3] = tem3;
  bar (x);
  return sum;
}
