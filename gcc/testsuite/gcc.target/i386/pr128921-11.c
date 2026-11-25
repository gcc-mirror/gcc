/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { { ! ia32 } && bitint } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* Same as pr128921-9.c, but with a _BitInt typedef.  */

typedef unsigned _BitInt(33) __attribute__((aligned(1))) bi33_a1;
struct S { char a; bi33_a1 b; };

char f (struct S s) { return s.a; }

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi" } } */
