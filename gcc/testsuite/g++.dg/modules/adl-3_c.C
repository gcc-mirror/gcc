// { dg-additional-options -fmodules-ts }

import inter;
import worker;

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

int main ()
{
  details::X x(2);

  if (fn (x) != 2) // { dg-error "not declared in" }
    return 1;

  // { dg-regexp "\n\[^\n]*adl-3_b.C:8:13: error: 'fn' was not declared in this scope$" }
  if (TPL (x) != 2) // { dg-message "required from here" }
    return 2;

  return 0;
}
