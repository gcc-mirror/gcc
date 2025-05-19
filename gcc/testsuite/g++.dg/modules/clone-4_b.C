// PR c++/120125
// { dg-additional-options "-fmodules -fdeclone-ctor-dtor" }

import M;

int main() {
  __shared_ptr<int> s1;
  __shared_ptr<double> s2;
}

// { dg-final { scan-assembler-not {_ZNW1M12__shared_ptrIiEC[1-4]Ev:} } }
// { dg-final { scan-assembler {_ZNW1M12__shared_ptrIdEC2Ev:} } }
