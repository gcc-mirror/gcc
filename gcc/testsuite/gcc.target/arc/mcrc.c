/* { dg-options "-mcrc" } */
/* { dg-do assemble } */

int f (int i)
{
  __asm__("crc %1, %1, %1" : "=r"(i) : "r"(i));
  return i;
}
