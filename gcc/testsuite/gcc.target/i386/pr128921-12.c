/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { { ! ia32 } && bitint } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* A packed union has alignment 1, but its _BitInt(33) member keeps
   alignment 8, so the member is misaligned at byte 1 of S and S must be
   passed in memory.  */

union __attribute__((packed)) U { unsigned _BitInt(33) x; };
struct __attribute__((packed)) S { char a; union U u; };

char
f (struct S s)
{
  return s.a;
}

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi|%sil|%esi|%rsi" } } */
