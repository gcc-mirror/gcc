// Make sure that we force an LSDA for a noexcept spec so
// that the personality routine will call terminate.  Also check that we
// optimize away the EH cleanup for var because the personality routine
// will call terminate before unwinding: there should not be an EH region
// (i.e. LEHB/LEHE labels) around the call to g().

// { dg-final { scan-assembler-not "_ZSt9terminatev" } }
// { dg-final { scan-assembler-not "EHB" } }
// { dg-final { scan-assembler "LSDA" } }

// { dg-options "-std=c++0x" }

struct A { ~A(); };
void g();
void f() noexcept
{
  A var;
  g();
}
