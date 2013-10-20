/* { dg-do compile } */
/* { dg-options "-mswape" } */
/* { dg-do assemble } */

int f (int i)
{
  __asm__("swape %1, %1" : "=r"(i) : "r"(i));
  return i;
}
