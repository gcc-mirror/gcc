/* PR c/51034 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

struct S;

int
main ()
{
  struct R { typeof (((struct W) {})) x; } r;	/* { dg-error "invalid use of undefined type" } */
  struct S { typeof (((struct S) {})) x; } s;	/* { dg-error "invalid use of undefined type" } */
  struct T { int x[sizeof ((struct T) {})]; } t;/* { dg-error "invalid use of undefined type" } */
  struct U { int x[sizeof((struct V){})];} u;	/* { dg-error "invalid use of undefined type" } */
}
