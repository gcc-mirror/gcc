/* Test for various combinations of const, arrays and typedefs:
   should never actually get const on the final array type, but
   all should end up in a read-only section.  PR c/12165.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-Wno-discarded-array-qualifiers" } */
/* The MMIX port always switches to the .data section at the end of a file.  */
/* { dg-final { scan-assembler-not "\\.data(?!\\.rel\\.ro)" { xfail powerpc*-*-aix* mmix-*-* x86_64-*-mingw* } } } */
/* { dg-final { scan-assembler-symbol-section {^_?a$} {^\.(const|rodata)} } } */
static const int a[2] = { 1, 2 };
/* { dg-final { scan-assembler-symbol-section {^_?a1$} {^\.(const|rodata)} } } */
const int a1[2] = { 1, 2 };
typedef const int ci;
/* { dg-final { scan-assembler-symbol-section {^_?b$} {^\.(const|rodata)} } } */
static ci b[2] = { 3, 4 };
/* { dg-final { scan-assembler-symbol-section {^_?b1$} {^\.(const|rodata)} } } */
ci b1[2] = { 3, 4 };
typedef int ia[2];
/* { dg-final { scan-assembler-symbol-section {^_?c$} {^\.(const|rodata)} } } */
static const ia c = { 5, 6 };
/* { dg-final { scan-assembler-symbol-section {^_?c1$} {^\.(const|rodata)} } } */
const ia c1 = { 5, 6 };
typedef const int cia[2];
/* { dg-final { scan-assembler-symbol-section {^_?d$} {^\.(const|rodata)} } } */
static cia d = { 7, 8 };
/* { dg-final { scan-assembler-symbol-section {^_?d1$} {^\.(const|rodata)} } } */
cia d1 = { 7, 8 };
/* { dg-final { scan-assembler-symbol-section {^_?e$} {^\.(const|rodata)} } } */
static cia e[2] = { { 1, 2 }, { 3, 4 } };
/* { dg-final { scan-assembler-symbol-section {^_?e1$} {^\.(const|rodata)} } } */
cia e1[2] = { { 1, 2 }, { 3, 4 } };
/* { dg-final { scan-assembler-symbol-section {^_?p$} {^\.(const|rodata)} } } */
void *const p = &a;
/* { dg-final { scan-assembler-symbol-section {^_?q$} {^\.(const|rodata)} } } */
void *const q = &b;
/* { dg-final { scan-assembler-symbol-section {^_?r$} {^\.(const|rodata)} } } */
void *const r = &c;
/* { dg-final { scan-assembler-symbol-section {^_?s$} {^\.(const|rodata)} } } */
void *const s = &d;
/* { dg-final { scan-assembler-symbol-section {^_?t$} {^\.(const|rodata)} } } */
void *const t = &e;
/* { dg-final { scan-assembler-symbol-section {^_?p1$} {^\.(const|rodata)} } } */
void *const p1 = &a1;
/* { dg-final { scan-assembler-symbol-section {^_?q1$} {^\.(const|rodata)} } } */
void *const q1 = &b1;
/* { dg-final { scan-assembler-symbol-section {^_?r1$} {^\.(const|rodata)} } } */
void *const r1 = &c1;
/* { dg-final { scan-assembler-symbol-section {^_?s1$} {^\.(const|rodata)} } } */
void *const s1 = &d1;
/* { dg-final { scan-assembler-symbol-section {^_?t1$} {^\.(const|rodata)} } } */
void *const t1 = &e1;
