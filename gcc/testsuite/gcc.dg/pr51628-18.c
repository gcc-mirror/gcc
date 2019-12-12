/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

void foo (int *);

int *
bar (int n, int k, void *ptr)
{
  struct A
  {
    int c[k];
    int x[n];
  } __attribute__ ((packed, aligned (4)));
  struct A *p = (struct A *) ptr;

  int *p0, *p1;
  p0 = p->x;
  foo (p0);
  p1 = &p->x[1];
  foo (p1);
  return &p->x[1];
}
