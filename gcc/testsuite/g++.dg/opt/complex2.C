// PR 22022
// { dg-do compile }
// { dg-options "-O2" }

_Complex float f();
_Complex float g();
_Complex float h()throw();
void i(_Complex float)throw();

void j(void)
{
  _Complex float x = h();
  try
  {
    try
    {
      x = f();
    }catch (...)
    {
      x = g();
    }
  }catch(...){}
  i(x);
}
