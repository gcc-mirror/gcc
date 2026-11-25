/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { { ! ia32 } && bitint } } } } */
/* { dg-options "-O2 -std=gnu2y -Wno-psabi" } */

/* A typedef of an enumeration with a bit-precise underlying type can
   lower the alignment, but the member is still checked against the
   natural alignment 8 of unsigned _BitInt(33): B is misaligned at byte
   1 of S, so S must be passed in memory.  */

enum E : unsigned _BitInt(33) { E0 };
typedef enum E E_a1 __attribute__((aligned(1)));
struct S { char a; E_a1 b; };

char
f (struct S s)
{
  return s.a;
}

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi|%sil|%esi|%rsi" } } */
