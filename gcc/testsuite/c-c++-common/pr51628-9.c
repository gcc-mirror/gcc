/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct pair_t
{
  int x;
  int i[4];
} __attribute__ ((packed));

extern struct pair_t p;
extern int *x;
extern void bar (int *);

int *addr = p.i;
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */

int *
foo (struct pair_t *p)
{
  int *p0, *p1;
  p0 = p->i;
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
  bar (p0);
  p1 = &p->i[1];
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
  bar (p1);
  bar (p->i);
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
  bar (&p->i[2]);
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
  x = p->i;
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
  return &p->i[3];
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}
