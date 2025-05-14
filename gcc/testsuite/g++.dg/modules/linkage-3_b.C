// { dg-do compile { target *-*-*gnu* } }
// { dg-additional-options "-fmodules" }
// { dg-final { scan-assembler "_ZW1M1x,comdat" } }

import M;

int main() {
  return x;
}
