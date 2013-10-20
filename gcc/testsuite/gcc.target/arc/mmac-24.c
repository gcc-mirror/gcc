/* { dg-do compile } */
/* { dg-options "-mmac-24" } */
/* { dg-do assemble } */

int f (int i)
{
  __asm__("mult %1, %1, %1" : "=r"(i) : "r"(i));
  return i;
}
