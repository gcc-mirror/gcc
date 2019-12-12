/* PR middle-end/89412 */

struct S { double a, b; } d;
int e;
double f;

void
foo ()
{
  _Complex double h;
  while (e)
    {
      f = h;
      *(struct S *) &h = d;
    }
}
