// PR c++/32121
// { dg-do compile }

void f (void)
{
  __label__ a, b;
  __label__ c;
  a:;
  b:;
  c:;
  {
    __label__ d;
    d:;
    if (0)
      {
	__label__ e;
	__label__ f;
	f:;
	e:;
      }
  }
}
