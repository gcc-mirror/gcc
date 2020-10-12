/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct A { int i; };
struct B { struct A a; };
struct C { struct B b __attribute__ ((packed)); };
/* { dg-warning "attribute ignored" "" { target default_packed } .-1 } */

extern struct C *p;

int*
g8 (void)
{
  return &p->b.a.i;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
}
