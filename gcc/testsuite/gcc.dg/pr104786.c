/* { dg-do compile } */
/* { dg-options "-std=gnu90" } */

void h(void *di, int num)
{
  char (*t)[num] = di;
  __asm__ ("" : "=X"( *t));
}
