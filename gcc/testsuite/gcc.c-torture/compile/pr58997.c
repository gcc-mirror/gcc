/* PR rtl-optimization/58997 */

int a, b, c, e;
short d;
char h;

void
foo ()
{
  while (b)
    {
      d = a ? c : 1 % a;
      c = d;
      h = d;
      if (!h)
	while (e)
	  ;
    }
}
