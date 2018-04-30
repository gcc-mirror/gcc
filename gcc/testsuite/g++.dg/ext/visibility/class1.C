// PR c++/26905
// Init should not be hidden, so calling it should use the PLT.

// { dg-options "-fpic" }
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-skip-if "" { *-*-darwin* } }
// { dg-require-visibility "" }
// { dg-require-effective-target fpic }
// { dg-final { scan-assembler "InitEv@PLT" } }

#pragma GCC visibility push(hidden)
struct __attribute__ ((visibility ("default"))) nsINIParser
{
    static void Init();
};

__attribute__ ((visibility ("default")))
void
CheckCompatibility(void)
{
  nsINIParser::Init();
}
