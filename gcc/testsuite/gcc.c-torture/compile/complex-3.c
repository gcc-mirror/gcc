/* PR 22116 */

void g(_Complex float);
_Complex float f(int data, _Complex float x, _Complex float y)
{
  _Complex float i, t;
  if (data) 
  {
    i = x +  __imag__ y;
    g(i);
  }
  else
    i = 5;
  t = x + __imag__ y;
  g(t);
  return t * i;
}
