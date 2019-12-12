/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O -Wno-incompatible-pointer-types" } */

struct B { int i; };
struct C { struct B b; } __attribute__ ((packed));

long* g8 (struct C *p) { return p; }
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
