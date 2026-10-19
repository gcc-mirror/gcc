/* PR rtl-optimization/127726 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-var-tracking -fcompare-debug --param=max-delay-slot-live-search=4" } */

/* Keep debug markers as DEBUG_INSNs through delay-slot reorg.  The
   live-search budget is just large enough without the marker below.  */

extern int g[64];
extern int h (int);

int
f0 (int a, int b, int c)
{
  int x = a;
  int y = b;
  int z = c;
  int t;

  if (a > 0)
    {
      t = a;
      x = h (x);
      y += 3;
    }
  else
    {
      z = z - x;
      g[57] = z;
      y = y | c;
      x = x + a;
    }
  if (y == 0)
    z = h (z) + x;
  return x + y * z;
}
