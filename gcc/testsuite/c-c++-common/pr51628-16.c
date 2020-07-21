/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct __attribute__ ((packed)) A { int i; };
struct B {
  struct A a;
} b;

int *p = (int*)&b.a.i;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
int *q = (int*)&b.a;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
