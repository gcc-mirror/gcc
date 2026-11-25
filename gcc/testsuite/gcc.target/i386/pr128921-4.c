/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* A packed union has alignment 1, but its member B keeps alignment 4, so
   B is misaligned at byte 1 of S and S must be passed in memory.  */

struct B { int b : 5; };
union __attribute__((packed)) U { struct B x; };
struct __attribute__((packed)) S { char a; union U u; };

char
f (struct S s)
{
  return s.a;
}

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi" } } */
