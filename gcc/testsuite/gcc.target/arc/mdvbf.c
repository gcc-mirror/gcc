/* { dg-do compile } */
/* { dg-options "-mdvbf" } */
/* { dg-do assemble } */

int f (int i)
{
  __asm__("vbfdw %1, %1" : "=r"(i) : "r"(i));
  return i;
}
