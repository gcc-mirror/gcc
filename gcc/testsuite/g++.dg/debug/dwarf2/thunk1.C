// Test that we don't add the x86 PC thunk to .debug_ranges
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-options "-g -fpic -fno-dwarf2-cfi-asm" }
// { dg-final { scan-assembler-times "LFB3" 5 } }

// Darwin doesn't use the thunk for PIC.
// { dg-skip-if "no pic thunk" { *-*-darwin* } }

template <class T> void f(T t) { }

int main()
{
  f(42);
}
