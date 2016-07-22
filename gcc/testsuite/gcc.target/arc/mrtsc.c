/* { dg-options "-mrtsc" } */
/* { dg-do assemble } */

int f (int i)
{
  __asm__("rtsc %1, %1" : "=r"(i) : "r"(i));
  return i;
}
