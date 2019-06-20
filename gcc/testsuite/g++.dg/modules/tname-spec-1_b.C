// { dg-additional-options -fmodules-ts }

import "tname-spec-1_a.H";
#include <typeinfo>

struct X
{
  using difference_type = char;
};

pointer_traits<X>::difference_type x;

int main ()
{
  return !(typeid (x) == typeid (long));
}
