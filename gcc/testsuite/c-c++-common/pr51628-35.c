/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct B { int i; };
struct C { struct B b; } __attribute__ ((packed));

extern struct C *p;
extern struct C *bar (void);

long *
foo1 (void)
{
  return (long *) p;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
}

long *
foo2 (void)
{
  return (long *) bar ();
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
}
