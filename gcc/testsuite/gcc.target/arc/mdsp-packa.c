/* { dg-do compile } */
/* { dg-options "-mdsp-packa" } */
/* { dg-do assemble } */

int f (int i)
{
  __asm__("minidl %1, %1, %1" : "=r"(i) : "r"(i));
  return i;
}
