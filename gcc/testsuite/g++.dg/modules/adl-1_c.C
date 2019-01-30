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

  if (TPL (x) != 2)
    return 1;

  if (TPL (y) != -2)
    return 2;

  return 0;
}
