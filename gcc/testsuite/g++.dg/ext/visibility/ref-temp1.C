// PR c++/91476
// Test that hidden and internal visibility propagates to reference temps.
// { dg-require-visibility "" }

#define HIDDEN __attribute((visibility("hidden")))

// { dg-final { scan-hidden "_ZGRZ1fvE3foo_" } }
HIDDEN inline const int* f() { static const int &foo = 1; return &foo; }

// { dg-final { scan-assembler-not "(weak|globl)\[^\n\]*_ZGRN12_GLOBAL__N_13fooE_" } }
namespace { const int &foo = 1; }

const void *volatile p;
int main()
{
  p = f();
  p = &foo;
}
