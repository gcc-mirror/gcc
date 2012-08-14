/* PR middle-end/53411 */

int a, b, c, d, e, f, g, h;
void fn1 (void);
int fn2 (void);

int
fn3 (x)
     int x;
{
  return a ? 0 : x;
}

void
fn4 (char x)
{
  int i, j, k;
  for (; e; e++)
    if (fn2 ())
      {
	f = 1;
	k = 0;
	for (; k <= 1; k++)
	  {
	    j = ~x;
	    i = f * j;
	    h = (fn3 (i | 0 <= c ^ 9L) != b | d) & 8;
	    g = x | 1;
	    fn1 ();
	  }
      }
  c = x;
}
