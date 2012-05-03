// Test that we don't add the x86 PC thunk to .debug_ranges
// { dg-do compile { target i?86-*-* } }
// { dg-options "-g -fpic" }
// { dg-final { scan-assembler-times "LFB3" 1 } }

template <class T> void f(T t) { }

int main()
{
  f(42);
}
