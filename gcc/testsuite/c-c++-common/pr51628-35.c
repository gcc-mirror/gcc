/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O -Wcast-align=strict" } */

struct B { int i; };
struct C { struct B b; } __attribute__ ((packed));

extern struct C *p;
extern struct C *bar (void);

long *
foo1 (void)
{
  return (long *) p;
/* { dg-warning "increases required alignment" "" { target { ! default_packed } } .-1 } */
}

long *
foo2 (void)
{
  return (long *) bar ();
/* { dg-warning "increases required alignment" "" { target { ! default_packed } } .-1 } */
}
