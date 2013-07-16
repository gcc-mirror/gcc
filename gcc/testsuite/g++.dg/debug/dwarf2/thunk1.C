// Test that we don't add the x86 PC thunk to .debug_ranges
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-require-effective-target fpic }
// { dg-skip-if "darwin doesn't use the thunk for PIC" { *-*-darwin* } }
// { dg-options "-g -fpic -fno-dwarf2-cfi-asm" }
// { dg-final { scan-assembler-times "LFB3" 5 } }

template <class T> void f(T t) { }

int main()
{
  f(42);
}
