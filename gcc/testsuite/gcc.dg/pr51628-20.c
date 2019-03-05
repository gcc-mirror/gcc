/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O -Wno-incompatible-pointer-types" } */

struct B { int i; };
struct C { struct B b; } __attribute__ ((packed));

extern struct C *p;

long* g8 (void) { return p; }
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
