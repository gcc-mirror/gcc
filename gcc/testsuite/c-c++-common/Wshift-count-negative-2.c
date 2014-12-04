/* { dg-do compile } */
/* { dg-options "-Wno-shift-count-negative" } */

void foo()
{
  unsigned i1 = 1U << -1;
  unsigned i2 = 1U >> -1;
}
