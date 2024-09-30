// { dg-additional-options -fmodules-ts }

// internal linkage symbol mangling is unspecified, but let's try and
// be unchanged from non-module internal mangling.

export module A;
// { dg-module-cmi A }

// { dg-final { scan-assembler {_ZL6addonev:} } }
static void addone () {}
// { dg-final { scan-assembler {_ZL1x:} } }
static int x = 5;

namespace {
// { dg-final { scan-assembler {_ZN12_GLOBAL__N_1L4frobEv:} } }
void frob () {}
// { dg-final { scan-assembler {_ZN12_GLOBAL__N_1L1yE:} } }
int y = 2;
struct Bill
{
  void F ();
};
// { dg-final { scan-assembler {_ZN12_GLOBAL__N_14Bill1FEv:} } }
void Bill::F() {}
}

// { dg-final { scan-assembler {_ZL4FrobPN12_GLOBAL__N_14BillE:} } }
static void Frob (Bill *b)
{
  if (b) b->F();
}

namespace N {
// { dg-final { scan-assembler {_ZN1NL5innerEv:} } }
static void inner() {}
// { dg-final { scan-assembler {_ZN1NL1zE:} } }
static int z = 3;
}

// { dg-final { scan-assembler {_ZW1A6addsixv:} } }
void addsix ()
{
  Frob(nullptr);
  frob();
  addone();
  void(x + y + N::z);
  N::inner();
}
