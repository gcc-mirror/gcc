/* PR middle-end/91190 */

unsigned a[1], c;
long d, h;
int e[2], f, g;
char i;

int
main ()
{
  char k = 0;
  int l;
  while (i || d)
    {
      if (g)
	while (1)
	  ;
      e[1] = 0;
      long m[2], n = ~(3 & (5 | (h | 9) * 2237420170));
      g = 90 * n;
      char b = m[300000000], j = 0;
      c = 5 ^ a[c ^ (b & 5)];
      int o = d;
      k = o ? : j;
      if (k)
	for (l = 0; l < 3; l++)
	  if (m[200000000000000000]) 
	    __builtin_printf ("%d", f);
    }
  return 0; 
}
