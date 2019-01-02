/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O -Wno-incompatible-pointer-types" } */

struct B { int i; };
struct C { struct B b; } __attribute__ ((packed));

int* g4 (struct C *p) { return &p->b; }
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
