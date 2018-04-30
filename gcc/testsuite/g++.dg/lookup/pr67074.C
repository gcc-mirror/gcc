// PR c++/67074 namespace aliases to the same place.

namespace P {
  namespace X {
    static int i = 1;
  }
}
namespace Q {
  namespace X = P::X;
}

using namespace P;
using namespace Q;

void Frob () { X::i; }

namespace N {}
namespace N = N;
