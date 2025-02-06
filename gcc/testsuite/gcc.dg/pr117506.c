/* PR rtl-optimization/117506 */
/* { dg-do compile } */
/* { dg-options "-O3 -funroll-loops" } */

char a;
int b;
unsigned c;
short d;

void
foo ()
{
  for (short f = 0; f < c; f += 3)
    {
      a ^= d;
      b = b < 0 ? b : 0;
    }
}
