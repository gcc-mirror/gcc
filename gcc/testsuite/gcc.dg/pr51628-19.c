/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

void foo (int *);

int *
bar (int n, int k, void *ptr)
{
  struct A
  {
    char c[k];
    int x[n];
  } __attribute__ ((packed));
  struct A *p = (struct A *) ptr;

  int *p0, *p1;
  p0 = p->x;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
  foo (p0);
  p1 = &p->x[1];
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
  foo (p1);
  return &p->x[1];
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
}
