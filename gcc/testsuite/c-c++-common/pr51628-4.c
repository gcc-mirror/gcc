/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct pair_t
{
  int x;
  int i;
} __attribute__((packed));

extern struct pair_t p;
extern int *x;
extern void bar (int *);

int *addr = &p.i;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */

int *
foo (void)
{
  struct pair_t arr[2] = { { 1, 10 }, { 2, 20 } };
  int *p0, *p1;
  p0 = &arr[0].i;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
  bar (p0);
  p1 = &arr[1].i;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
  bar (p1);
  bar (&p.i);
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
  x = &p.i;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
  return &p.i;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
}
