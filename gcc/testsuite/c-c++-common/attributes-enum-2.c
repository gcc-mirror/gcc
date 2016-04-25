/* Test enumerators with attributes.  Test invalid uses.  */
/* PR c/47043 */
/* { dg-do compile } */

enum E {
  A __attribute__((foo)),	/* { dg-warning "ignored" } */
  B __attribute__((cold)),	/* { dg-warning "ignored" } */
  C __attribute__((const)),	/* { dg-warning "ignored" } */
  D __attribute__((unused)),
  E __attribute__((flatten)),	/* { dg-warning "ignored" } */
  F __attribute__((tm)),	/* { dg-warning "ignored" } */
  G __attribute__((common)),	/* { dg-warning "ignored" } */
  H __attribute__((volatile)),	/* { dg-warning "ignored" } */
};
