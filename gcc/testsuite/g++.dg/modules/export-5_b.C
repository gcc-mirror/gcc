// PR c++/114917
// { dg-additional-options "-fmodules-ts" }

import M;

int main() {
  ns::S<double*>::a x{};
  ns::S<int*>::b y{};
  ns::S<int> z{};

  ns::foo<double>();
  ns::foo<int>();
}
