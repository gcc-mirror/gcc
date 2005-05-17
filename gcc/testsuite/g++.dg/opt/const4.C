// PR c++/21454
// Test whether A is put into .rodata section on platforms
// that have it.
// { dg-do compile }

const int a[] __attribute__ ((__used__)) = { 0, 1, 2, 3 };

// The MMIX port always switches to the .data section at the end of a file.
// { dg-final { scan-assembler-not "\\.data(?!\\.rel\\.ro)" { xfail powerpc*-*-aix* mmix-*-* } } }
