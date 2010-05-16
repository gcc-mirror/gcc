// Make sure that we force an LSDA for a throw() spec with -fnothrow-opt so
// that the personality routine will call terminate.

// { dg-final { scan-assembler-not "_ZSt9terminatev" } }
// { dg-final { scan-assembler-not "EHB" } }
// { dg-final { scan-assembler "LSDA" } }

// { dg-options "-fnothrow-opt" }

void g();
void f() throw() { g(); }
