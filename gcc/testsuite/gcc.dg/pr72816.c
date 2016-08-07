/* PR c/72816 */
/* { dg-do compile } */
/* { dg-options "-std=gnu11" } */

typedef const int A[];
struct S {
  int a;
  A b;	/* { dg-error "array size missing" } */
};
