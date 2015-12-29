/* PR rtl-optimization/55151 */
/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-fPIC" } */

int a, b, c, d, e, f, g, h, i, j, k, l;
void f4 (void)
{
  __asm__ volatile ("":[a] "=r,m" (a),[b] "=r,m" (b),[c] "=r,m" (c),
		    [d] "=r,m" (d),[e] "=r,m" (e),[f] "=r,m" (f),
		    [g] "=r,m" (g),[h] "=r,m" (h),[i] "=r,m" (i),
		    [j] "=r,m" (j),[k] "=r,m" (k),[l] "=r,m" (l):"[a],m" (a),
		    "[j],m" (j), "[k],m" (k), "[l],m" (l));
}
