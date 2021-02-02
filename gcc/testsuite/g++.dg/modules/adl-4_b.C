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

  // unexported hidden::fn@inter is not visible from TPL@inter
  if (TPL (y) != -2)
    return 2;

  return 0;
}

// ADL fails
// { dg-regexp {[^\n]*/adl-4_a.C:14:[0-9]*: error: 'fn' was not declared in this scope\n} }
