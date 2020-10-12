/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O -Wno-incompatible-pointer-types" } */

struct A {
  int i;
} __attribute__ ((packed));

long* f8 (struct A *p) { return &p->i; }
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
