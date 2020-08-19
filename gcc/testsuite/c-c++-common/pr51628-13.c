/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct B { int i; };
struct C { struct B b; } __attribute__ ((packed));

int* h4 (struct C *p) { return &p->b.i; }
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
