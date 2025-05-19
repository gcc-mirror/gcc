// PR c++/120349
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M }

module;

// GMF types; should have vtables emitted in importers
struct BGG {
  virtual inline ~BGG() {}
};
struct BGM {
  virtual inline ~BGM() {}
};
struct DGG : BGG {};

export module M;

export using ::DGG;

// Module-local types; should have vtables emitted here
struct BM {
  virtual inline ~BM() {}
};
export struct DGM : BGM {};  // note: this emits BGM's vtable here too
export struct DM : BM {};

// { dg-final { scan-assembler-not "_ZTV3BGG:" } }
// { dg-final { scan-assembler "_ZTV3BGM:" } }
// { dg-final { scan-assembler "_ZTVW1M2BM:" } }
