// PR c++/122421
// { dg-additional-options "-fmodules" }

import M;

int main() {
  const int& a = Type<int>::arr[0];
  const int& b = Type<double>::arr[0];
}

// { dg-final { scan-assembler {_ZNW1M4TypeIiE3arrE:} } }
// { dg-final { scan-assembler-not {_ZNW1M4TypeIdE3arrE:} } }
