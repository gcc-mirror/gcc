// PR c++/44127

// This is basically the same test as g++.eh/terminate1.C, but that one
// tests runtime behavior and this tests the assembly output.  The test
// should call terminate (because initializing the catch parm throws), but
// from the personality routine, not directly.

// { dg-final { scan-assembler-not "_ZSt9terminatev" } }

// Also there should only be two EH call sites: #0 for throw A() and #1 for
// _Unwind_Resume.  We don't want call site info for __cxa_end_catch, since
// ~A is trivial.

// { dg-final { scan-assembler-not "LEHB2" } }

struct A
{
  A() { }
  A (const A&) { throw 1; }
};

int main()
{
  try
    {
      throw A();
    }
  catch (A) { }
}
