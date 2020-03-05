// PR c++/91476
// Test that hidden and internal visibility propagates to reference temps.

// { dg-final { scan-assembler-not "(weak|globl)\[^\n\]*_ZGRN12_GLOBAL__N_13fooE_" } }
namespace { const int &foo = 1; }

const void *volatile p;
int main()
{
  p = &foo;
}
