/* PR tree-optimization/109115 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int a, b;

int
main ()
{
  unsigned short c = a, e = -1;
  if (b)
    {
      unsigned d = (a ^ 1U) / a & c;
      int f = (~d >> ~a) / e;
      if (a)
	f = a;
      a = f;
    }
  return 0;
}
