// { dg-additional-options "-fmodules-ts" }
import One;

int main ()
{
  base b (0xfeed);
  if (!(b.b == 0xfeed))
    return 1;

  derived d (0xcafe, 0xbeef);
  if (!(d.b == 0xcafe && d.d == 0xbeef))
    return 2;

  return 0;
}
