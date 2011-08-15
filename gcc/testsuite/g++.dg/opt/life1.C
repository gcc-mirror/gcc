// This testcase did not set up the pic register on IA-32 due
// to bug in calculate_global_regs_live EH edge handling.
// { dg-do compile { target { { i?86-*-linux* x86_64-*-linux* } && ia32 } } }
// { dg-require-effective-target fpic }
// { dg-options "-O2 -fPIC" }

struct A { };

void foo (A (*fn)())
{
  try {
    A a = fn ();
  } catch (...) {
  }
}

// { dg-final { scan-assembler "GLOBAL_OFFSET_TABLE" } }
