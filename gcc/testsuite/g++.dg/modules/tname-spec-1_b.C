// { dg-additional-options -fmodules-ts }

#include <typeinfo>
import "tname-spec-1_a.H";

struct X
{
  using difference_type = char;
};

pointer_traits<X>::difference_type x;

int main ()
{
  return !(typeid (x) == typeid (long));
}
