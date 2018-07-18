/* { dg-options "-mswape" } */
/* { dg-do assemble } */
/* { dg-skip-if "" { arc6xx } } */

int f (int i)
{
  __asm__("swape %1, %1" : "=r"(i) : "r"(i));
  return i;
}
