// PR 22022
// { dg-do compile }
// { dg-options "-O2" }

_Complex float f();
_Complex float g();
_Complex float h()throw();
void i(float)throw();

float j(void)
{
  _Complex float x = h();
  try
  {
    try
    {
      x = f();
    }catch (...)
    {
      x += g();
    }
  }catch(...){}
  i(__builtin_crealf(x)+__builtin_cimagf(x));
}
