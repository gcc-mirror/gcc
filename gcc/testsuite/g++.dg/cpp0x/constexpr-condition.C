// { dg-options -std=c++0x }
// Core DR 948

constexpr int something() { return 3; }

int main() {
  if (constexpr long v = something()) {}
  if (static long v = something()) { } // { dg-error "decl-specifier invalid" }
}
