// PR c++/107688
// { dg-additional-options "-fmodules-ts" }

import M;

using namespace Empty;

int main() {
  S<int> x;
  S<int*>::b y;
  S<int**>::a z;
  foo();
}
