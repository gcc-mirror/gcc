/* PR120381 */
/* { dg-do compile } */

struct A {
  struct A {		/* { dg-error "nested redefinition" } */
    struct A *p;
  } *p;
};
int foo(const struct A *q) { return q->p == q; }

