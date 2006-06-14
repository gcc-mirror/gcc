// PR c++/27227

namespace x {
  extern "C" const int y;
}
using x::y;
extern "C" int const y=0;
