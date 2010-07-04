// Make sure that we force an LSDA for a throw() spec with -fnothrow-opt so
// that the personality routine will call terminate.  Also check that we
// optimize away the EH cleanup for var because the personality routine
// will call terminate before unwinding: there should not be an EH region
// (i.e. LEHB/LEHE labels) around the call to g().

// { dg-final { scan-assembler-not "_ZSt9terminatev" } }
// { dg-final { scan-assembler-not "EHB" } }
// { dg-final { scan-assembler "LSDA" } }

// { dg-options "-fnothrow-opt" }

struct A { ~A(); };
void g();
void f() throw()
{
  A var;
  g();
}
