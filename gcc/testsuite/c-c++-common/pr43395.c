/* PR c/43395 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */

void *
foo (void)
{
 lab:
  return &&lab;
/* { dg-warning "address of label" "" { target *-*-* } .-1 } */
}

void *
bar (void)
{
  __label__ lab;
 lab:
  return &&lab;
/* { dg-warning "address of label" "" { target *-*-* } .-1 } */
}

void *
baz (void)
{
  int i;
  return &i;
/* { dg-warning "address of local variable" "" { target *-*-* } .-1 } */
}
