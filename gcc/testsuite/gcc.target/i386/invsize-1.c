/* { dg-do compile { target ia32 } } */

void foo (char x)
{
  register char rx __asm ("si") = x;

  __asm__ volatile ("# %0" : : "r" (rx));
} /* { dg-error "unsupported size" }  */
