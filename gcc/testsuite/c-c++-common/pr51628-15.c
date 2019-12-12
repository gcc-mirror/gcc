/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct A {
  int i;
} __attribute__ ((packed));

int*
f (struct A *p, int *q)
{
  return q ? q : &p->i;
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}
