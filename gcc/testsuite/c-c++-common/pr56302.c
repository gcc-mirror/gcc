/* PR c++/56302 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

void
foo (int x)
{
  __asm__ __volatile__ ("" : : "n" (-1 * (int) sizeof (&x)));
}
