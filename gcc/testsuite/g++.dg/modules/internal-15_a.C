// { dg-additional-options "-fmodules -fdump-lang-module-graph -Wno-global-module" }
// { dg-module-cmi A }

export module A;

namespace N {
  struct A {};
  void adl(A);
  inline namespace inner {
    static void adl(int);
  }
}
namespace G {
  struct B {};
  void adl(B);
  namespace {
    extern "C++" void adl(int);
  }
}
void adl(double);

template <typename T>
inline void h(T t) {
  adl(t);
}

// { dg-final { scan-lang-dump {Binding on tu-local function_decl:'::N::inner::adl' found} module } }
// { dg-final { scan-lang-dump-not {'G::_GLOBAL__N_1::adl'} module } }
