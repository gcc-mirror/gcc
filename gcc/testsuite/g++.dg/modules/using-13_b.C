// { dg-additional-options "-fmodules-ts" }

import M;

int main() {
  A a;

  // Check all specialisations are correctly exported
  B<void> b;
  B<int>::foo b1;
  B<int*>::bar b2;

  C c;

  auto d = D;

#if __cpp_concepts >= 201907L
  auto e = E<void>;
#endif
}
