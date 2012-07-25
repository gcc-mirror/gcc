// PR c++/40476

namespace A
{
  int i;			// { dg-message "i" }
}
using namespace A;
namespace B
{
  namespace B2
  {
    int i;			// { dg-message "i" }
  }
  using namespace B2;
}
using namespace B;

int j = ::i;			// { dg-error "ambiguous" }
