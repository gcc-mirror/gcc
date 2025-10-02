// { dg-additional-options -fmodules-ts }

import inter;

namespace hidden
{

struct Y
{

  int m;
  Y (int m) : m(m)
  {
  }

  operator int () const 
  {
    return m;
  }
};

}

int main ()
{
  hidden::Y y(2);

  // unexported hidden::fn@inter is visible from TPL@inter because it's a
  // dependent name and so lookup is performed at each point on the
  // instantiation context, including the point at the end of inter.
  if (TPL (y) != -2)
    return 2;

  return 0;
}
