/* { dg-do compile { target int128 } } */
/* { dg-options "" } */

void foo (__int128 x)
{
  __asm__ volatile ("# %0" : : "r" (x));
} /* { dg-warning "unsupported size" }  */
