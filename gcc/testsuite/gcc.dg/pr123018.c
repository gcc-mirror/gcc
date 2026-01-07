/* PR c/123018 */
/* { dg-do compile } */

struct A {
  int x : 8 __attribute__ ((vector_size (8)));	/* { dg-error "bit-field 'x' has invalid type" } */
};
struct B {
  float x : 8;					/* { dg-error "bit-field 'x' has invalid type" } */
};
struct C {
  int : 8 __attribute__ ((vector_size (8)));	/* { dg-error "bit-field '\[^\n\r]*anonymous\[^\n\r]*' has invalid type" } */
  int x;
};
struct D {
  float : 8;					/* { dg-error "bit-field '\[^\n\r]*anonymous\[^\n\r]*' has invalid type" } */
  int x;
};
