/* PR middle-end/25521 - place `const volatile' objects in read-only
   sections.

   { dg-do compile }
   { dg-require-effective-target elf }
   { dg-skip-if "" { ! const_volatile_readonly_section } } */

const volatile int foo = 30;

/* { dg-final { scan-assembler {.section C,} { target { rx-*-* } } } } */
/* { dg-final { scan-assembler-symbol-section {^_?foo$} {^\.(const|s?rodata)} { target { ! "rx-*-*" } } } } */
