/* Test for various combinations of const, arrays and typedefs:
   should never actually get const on the final array type, but
   all should end up in a read-only section.  PR c/12165.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* The MMIX port always switches to the .data section at the end of a file.  */
/* { dg-final { scan-assembler-not "\\.data(?!\\.rel\\.ro)" { xfail powerpc*-*-aix* mmix-*-* } } } */
static const int a[2] = { 1, 2 };
const int a1[2] = { 1, 2 };
typedef const int ci;
static ci b[2] = { 3, 4 };
ci b1[2] = { 3, 4 };
typedef int ia[2];
static const ia c = { 5, 6 };
const ia c1 = { 5, 6 };
typedef const int cia[2];
static cia d = { 7, 8 };
cia d1 = { 7, 8 };
static cia e[2] = { { 1, 2 }, { 3, 4 } };
cia e1[2] = { { 1, 2 }, { 3, 4 } };
void *const p = &a;
void *const q = &b;
void *const r = &c;
void *const s = &d;
void *const t = &e;
void *const p1 = &a1;
void *const q1 = &b1;
void *const r1 = &c1;
void *const s1 = &d1;
void *const t1 = &e1;
