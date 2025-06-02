// PR c++/120349
// { dg-additional-options "-fmodules -Wno-global-module" }

import M;

int main() {
  DGG dgg;
  DGM dgm;
  DM dm;
}

// { dg-final { scan-assembler "_ZTV3BGG:" } }
// { dg-final { scan-assembler "_ZTV3BGM:" } }
// { dg-final { scan-assembler-not "_ZTVW1M2BM:" } }
