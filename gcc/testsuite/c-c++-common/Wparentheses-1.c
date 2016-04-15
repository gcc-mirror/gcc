/* PR c/70436 */
/* { dg-do compile } */
/* { dg-options "-Wparentheses" } */

int a, b, c[101], d[101], e[101], f[101];

void
f1 (void)
{
  int i;

  if (a) /* { dg-warning "ambiguous" } */
    #pragma GCC ivdep
    for (i = 0; i < 100; i++)
      if (b)
	c[i] = d[i] + e[i];
      else
	f[i] = d[i] * e[i];

  if (a)
    #pragma GCC ivdep
    for (i = 0; i < 100; i++)
      {
	if (b)
	  c[i] = d[i] + e[i];
	else
	  f[i] = d[i] * e[i];
      }

  if (a)
    #pragma GCC ivdep
    for (i = 0; i < 100; i++)
      {
	if (b)
	  c[i] = d[i] + e[i];
      }
  else
    f[i] = d[i] * e[i];
}
