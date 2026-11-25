/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2" } */

/* The aligned attribute of the typedef raises the alignment of the type
   variant, but the member is aligned to the natural alignment of its type
   and its int member is aligned, so it must still be passed in registers
   and there is no ABI change, hence no note.  */

struct A { int x; };
typedef struct A __attribute__((aligned(8))) A_a8;
struct __attribute__((packed)) S { int pad; A_a8 a; };

int f (struct S s) { return s.a.x + s.pad; }

/* { dg-bogus "the ABI of passing" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler-not "\\(%\[er\]sp\\)" } } */
