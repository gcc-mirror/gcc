// { dg-additional-options "-fmodules" }
// The different declarations in the anonymous namespace shouldn't clash with
// those in M.

namespace {
  using A = double;
  typedef double B;
  using C = double;
  typedef double D;
}
import M;
int main() {
  A a = 1.0;
  B b = 2.0;
  C c = 3.0;
  D d = 4.0;
}
