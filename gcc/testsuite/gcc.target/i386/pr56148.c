/* PR inline-asm/56148 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (void)
{
  unsigned char e[16];
  unsigned long a, b, c, d;
  __asm__ __volatile__ ("" : "=d" (a), "=&c" (c), "=&D" (d), "=&a" (b)
               : "0" (-1U), "mr" (e), "1" (128 >> 5), "2" (e), "3" (-1U));
}
