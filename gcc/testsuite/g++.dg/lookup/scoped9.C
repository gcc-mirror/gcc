// PR c++/71173

namespace foo {
  namespace bar {
    class foo {};
  }
  class baz {};
}
using namespace foo::bar;
::foo::baz mybaz;
