/* PR rtl-optimization/41033 */
/* { dg-do run } */
/* { dg-options "-O1 -fno-strict-aliasing" } */

struct X {
  int i;
  int j;
};

int foo(struct X *p, struct X *q)
{
  p->j = 1;
  q->i = 0;
  return p->j;
}

extern void abort (void);

int main()
{
  struct X x;
  if (foo (&x, (struct X *)&x.j) != 0)
    abort ();
  return 0;
}
