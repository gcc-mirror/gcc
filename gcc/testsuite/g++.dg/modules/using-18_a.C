// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }
// Test revealing non-exported declarations still prevents
// needed GMF declarations from being discarded

module;

struct A {};
int f();

namespace ns {
  struct B {};
  int g();
}

extern "C" int h();
namespace ns {
  extern "C" int h();
}

export module M;

using ::A;
using ::f;

using ns::B;
using ns::g;

using ns::h;
