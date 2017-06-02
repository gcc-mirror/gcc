/* PR c/43395 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */

void *
foo (void)
{
 lab: /* { dg-line foo_lab } */
  return &&lab;
/* { dg-warning "function returns address of label" "" { target c } .-1 } */
/* { dg-warning "address of label" "" { target c++ } foo_lab } */
}

void *
bar (void)
{
  __label__ lab;
 lab: /* { dg-line bar_lab } */
  return &&lab;
/* { dg-warning "function returns address of label" "" { target c } .-1 } */
/* { dg-warning "address of label" "" { target c++ } bar_lab } */
}

void *
baz (void)
{
  int i; /* { dg-line baz_i } */
  return &i;
/* { dg-warning "function returns address of local variable" "" { target c } .-1 } */
/* { dg-warning "address of local variable" "" { target c++ } baz_i } */
}
