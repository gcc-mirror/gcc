// { dg-additional-options "-fmodules-ts" }
// Test changes for https://github.com/itanium-cxx-abi/cxx-abi/pull/171

export module M;

// Attached to module, should only be emitted in this TU
// { dg-final { scan-assembler {_ZTVW1M8Attached:} } }
// { dg-final { scan-assembler {_ZTIW1M8Attached:} } }
// { dg-final { scan-assembler {_ZTSW1M8Attached:} } }
export struct Attached {
  virtual void key();
};

// Not attached to module, should be emitted where key function is
// { dg-final { scan-assembler-not {_ZTV10Unattached:} } }
// { dg-final { scan-assembler-not {_ZTI10Unattached:} } }
// { dg-final { scan-assembler-not {_ZTS10Unattached:} } }
export extern "C++" struct Unattached {
  // Key function not defined here
  virtual void key();
};

// Template, should be emitted everywhere it's used
export template <typename T> struct Templated {
  virtual void key() {}
};

// { dg-final { scan-assembler {_ZTVW1M9TemplatedIiE:} } }
// { dg-final { scan-assembler {_ZTIW1M9TemplatedIiE:} } }
// { dg-final { scan-assembler {_ZTSW1M9TemplatedIiE:} } }
static Templated<int> x;
