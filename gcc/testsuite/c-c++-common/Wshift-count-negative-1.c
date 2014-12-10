/* { dg-do compile } */
/* { dg-options "-Wshift-count-negative" } */

void foo()
{
  unsigned i1 = 1U << -1; /* { dg-warning "left shift count is negative" } */
  unsigned i2 = 1U >> -1; /* { dg-warning "right shift count is negative" } */
}
