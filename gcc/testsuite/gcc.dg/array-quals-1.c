/* Test for various combinations of const, arrays and typedefs:
   should never actually get const on the final array type, but
   all should end up in a read-only section.  PR c/12165.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-Wno-discarded-array-qualifiers" } */
/* { dg-additional-options "-fno-pie" { target pie } } */
/* The MMIX port always switches to the .data section at the end of a file.  */
/* { dg-final { scan-assembler-not "\\.data(?!\\.rel\\.ro)" { xfail powerpc*-*-aix* mmix-*-* x86_64-*-mingw* } } } */
/* { dg-final { scan-assembler-symbol-section {^_?a$} {^\.(const|rodata|srodata)|\[RO\]} } } */
static const int a[2] = { 1, 2 };
/* { dg-final { scan-assembler-symbol-section {^_?a1$} {^\.(const|rodata|srodata|sdata)|\[RO\]} } } */
const int a1[2] = { 1, 2 };
typedef const int ci;
/* { dg-final { scan-assembler-symbol-section {^_?b$} {^\.(const|rodata|srodata)|\[RO\]} } } */
static ci b[2] = { 3, 4 };
/* { dg-final { scan-assembler-symbol-section {^_?b1$} {^\.(const|rodata|srodata|sdata)|\[RO\]} } } */
ci b1[2] = { 3, 4 };
typedef int ia[2];
/* { dg-final { scan-assembler-symbol-section {^_?c$} {^\.(const|rodata|srodata)|\[RO\]} } } */
static const ia c = { 5, 6 };
/* { dg-final { scan-assembler-symbol-section {^_?c1$} {^\.(const|rodata|srodata|sdata)|\[RO\]} } } */
const ia c1 = { 5, 6 };
typedef const int cia[2];
/* { dg-final { scan-assembler-symbol-section {^_?d$} {^\.(const|rodata|srodata)|\[RO\]} } } */
static cia d = { 7, 8 };
/* { dg-final { scan-assembler-symbol-section {^_?d1$} {^\.(const|rodata|srodata|sdata)|\[RO\]} } } */
cia d1 = { 7, 8 };
/* { dg-final { scan-assembler-symbol-section {^_?e$} {^\.(const|rodata|srodata)|\[RO\]} } } */
static cia e[2] = { { 1, 2 }, { 3, 4 } };
/* { dg-final { scan-assembler-symbol-section {^_?e1$} {^\.(const|rodata|srodata|sdata)|\[RO\]} } } */
cia e1[2] = { { 1, 2 }, { 3, 4 } };
/* { dg-final { scan-assembler-symbol-section {^_?p$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const p = &a;
/* { dg-final { scan-assembler-symbol-section {^_?q$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const q = &b;
/* { dg-final { scan-assembler-symbol-section {^_?r$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const r = &c;
/* { dg-final { scan-assembler-symbol-section {^_?s$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const s = &d;
/* { dg-final { scan-assembler-symbol-section {^_?t$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const t = &e;
/* { dg-final { scan-assembler-symbol-section {^_?p1$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const p1 = &a1;
/* { dg-final { scan-assembler-symbol-section {^_?q1$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const q1 = &b1;
/* { dg-final { scan-assembler-symbol-section {^_?r1$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const r1 = &c1;
/* { dg-final { scan-assembler-symbol-section {^_?s1$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const s1 = &d1;
/* { dg-final { scan-assembler-symbol-section {^_?t1$} {^\.(const|rodata|srodata|sdata)|\[RW\]} } } */
void *const t1 = &e1;
