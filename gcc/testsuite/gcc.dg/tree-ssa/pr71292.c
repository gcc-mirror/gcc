/* PR middle-end/71292 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long a;
long b, d;
int c;
void fn1 ()
{
  unsigned long e = a + c;
  b = d + e + a + 8;
}
