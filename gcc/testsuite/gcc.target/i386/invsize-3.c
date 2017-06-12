/* { dg-do compile } */
/* { dg-options "" } */

void foo (long double x)
{
  __asm__ volatile ("# %0" : : "r" (x));
} /* { dg-warning "unsupported size" }  */
