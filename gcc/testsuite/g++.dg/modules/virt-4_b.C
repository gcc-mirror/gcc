// { dg-additional-options "-fmodules-ts" }

module M;

// Attached to module, shouldn't be defined in this TU
// (was already emitted in interface unit)
// { dg-final { scan-assembler-not {_ZTVW1M8Attached:} } }
// { dg-final { scan-assembler-not {_ZTIW1M8Attached:} } }
// { dg-final { scan-assembler-not {_ZTSW1M8Attached:} } }
void Attached::key() {}

// Not attached to module and this is the key function,
// so vtables and RTTI should be emitted here
// { dg-final { scan-assembler {_ZTV10Unattached:} } }
// { dg-final { scan-assembler {_ZTI10Unattached:} } }
// { dg-final { scan-assembler {_ZTS10Unattached:} } }
extern "C++" void Unattached::key() {}

// Template vtables should be emitted wherever it's used
// { dg-final { scan-assembler {_ZTVW1M9TemplatedIiE:} } }
// { dg-final { scan-assembler {_ZTIW1M9TemplatedIiE:} } }
// { dg-final { scan-assembler {_ZTSW1M9TemplatedIiE:} } }
static Templated<int> y;
