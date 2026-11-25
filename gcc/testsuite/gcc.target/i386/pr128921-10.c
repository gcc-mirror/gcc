/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* Same as pr128921-9.c, but with an array of such typedef elements.  */

struct B { int b : 5; };
typedef struct B __attribute__((aligned(1))) B_a1;
struct S { char a; B_a1 x[2]; };

char f (struct S s) { return s.a; }

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi|%sil|%esi|%rsi" } } */
