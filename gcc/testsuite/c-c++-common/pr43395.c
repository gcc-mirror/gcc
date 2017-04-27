/* PR c/43395 */
/* { dg-do compile } */

void *
foo (void)
{
lab:
  return &&lab;
/* { dg-warning "function returns address of label" "" { target c } .-1 } */
/* { dg-warning "address of label" "" { target c++ } 7 } */
}

void *
bar (void)
{
  __label__ lab;
lab:
  return &&lab;
/* { dg-warning "function returns address of label" "" { target c } .-1 } */
/* { dg-warning "address of label" "" { target c++ } 17 } */
}

void *
baz (void)
{
  int i;
  return &i;
/* { dg-warning "function returns address of local variable" "" { target c } .-1 } */
/* { dg-warning "address of local variable" "" { target c++ } 26 } */
}
