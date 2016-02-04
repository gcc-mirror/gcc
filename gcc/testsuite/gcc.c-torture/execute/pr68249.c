/* PR rtl-optimization/68249 */

int a, b, c, g, k, l, m, n;
char h;

void
fn1 ()
{
  for (; k; k++)
    {
      m = b || c < 0 || c > 1 ? : c;
      g = l = n || m < 0 || (m > 1) > 1 >> m ? : 1 << m;
    }
  l = b + 1;
  for (; b < 1; b++)
    h = a + 1;
}

int
main ()
{
  char j; 
  for (; a < 1; a++)
    {
      fn1 ();
      if (h)
	j = h;
      if (j > c)
	g = 0;
    }

  if (h != 1) 
    __builtin_abort (); 

  return 0;
}
