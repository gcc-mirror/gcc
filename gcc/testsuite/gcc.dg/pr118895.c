/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long a;
void fn1()
{
  unsigned long e = a;
  int c = e;
  int d = c < 100 ? c : 0;
  if (d + (int)e & 608)
    while (e & 608)
      e <<= 1;
}
