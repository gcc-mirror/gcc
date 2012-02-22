/* { dg-do compile } */
/* { dg-options "-O0" } */

void foo (int a)
{
  asm volatile ("# %H0" : : "r" (a));  /* { dg-error "not an offsettable" } */
}
