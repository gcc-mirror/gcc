/* { dg-do compile { target ia32 } } */
/* { dg-options "" } */

void foo (long long x)
{
  __asm__ volatile ("# %0" : : "r" (x));
} /* { dg-warning "unsupported size" }  */
