// PR c++/44127

// This is basically the same test as g++.eh/terminate1.C, but that one
// tests runtime behavior and this tests the assembly output.  The test
// should call terminate (because initializing the catch parm throws), but
// from the personality routine, not directly.

// { dg-final { scan-assembler-not "_ZSt9terminatev" } }

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
