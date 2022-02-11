// PR c++/93477
// { dg-require-weak }

namespace x {
  struct s {
    s();
    static int a;
  };
  s::s() {}
  // { dg-final { scan-assembler {.weak[^\n]*_ZGVN1x1bE} } }
  struct s __attribute__((weak)) b = s();
}
