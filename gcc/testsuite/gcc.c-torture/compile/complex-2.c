/* PR 22103 */

_Complex float f(void);
void *a;

_Complex float g(void)
{
  _Complex float x = f();
  __imag__ x = 1.0;
  if (__imag__ x != 1.0)
    {
      a = &x;
    }
  return x;
}

