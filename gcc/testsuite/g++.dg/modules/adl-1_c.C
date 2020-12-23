// { dg-additional-options -fmodules-ts }

import inter;

namespace details
{

struct X 
{

  int m;
  X (int m) : m(m)
  {
  }

  operator int () const 
  {
    return m;
  }
};

}

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
  details::X x(2);
  hidden::Y y(2);

  // details::fn@worker is visible from TPL@inter
  if (TPL (x) != 2) // instantiate TPL<details::X>(T&)
    return 1;

  // hidden::fn@inter is visible from TPL@inter
  if (TPL (y) != -2) // instantiate TPL<hidden::Y>(T&)
    return 2;

  return 0;
}
