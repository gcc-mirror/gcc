// { dg-module-do link }
// { dg-additional-options "-fmodules-ts" }

import M;

int main() {
  A a(0);
}

// { dg-final { scan-assembler-not {_ZTTW1M1A:} } }
// { dg-final { scan-assembler-not {_ZTVW1M1A:} } }
