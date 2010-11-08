struct A
{
  virtual A* f();
};

struct B: virtual A
{
  virtual A* f();
};

struct C: B
{
  virtual C* f();
};

C* C::f() { return 0; }

// When we emit C::f, we should emit both thunks: one for B and one for A.
// { dg-final { scan-assembler "_ZTch0_v0_n16_N1C1fEv" { target { ilp32  && { ! { ia64-*-hpux* } } } } } }
// { dg-final { scan-assembler "_ZTch0_v0_n32_N1C1fEv" { target { lp64  || { ia64-*-hpux* } } } } }
// { dg-final { scan-assembler "_ZTcv0_n12_v0_n16_N1C1fEv" { target { ilp32  && { ! { ia64-*-hpux* } } } } } }
// { dg-final { scan-assembler "_ZTcv0_n24_v0_n32_N1C1fEv" { target { lp64  || { ia64-*-hpux* } } } } }

struct D: B
{
  virtual void dummy ();
  virtual D* f();
};

void D::dummy() { }

// When we emit the D vtable, it should refer to the thunk for B.
// { dg-final { scan-assembler "_ZTch0_v0_n16_N1D1fEv" { target { ilp32  && { ! { ia64-*-hpux* } } } } } }
// { dg-final { scan-assembler "_ZTch0_v0_n32_N1D1fEv" { target { lp64  || { ia64-*-hpux* } } } } }
