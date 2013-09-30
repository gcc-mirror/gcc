/* { dg-do compile } */
/* { dg-options "-mmac-d16" } */
/* { dg-do assemble } */

int f (int i)
{
  __asm__("muldw %1, %1, %1" : "=r"(i) : "r"(i));
  return i;
}
