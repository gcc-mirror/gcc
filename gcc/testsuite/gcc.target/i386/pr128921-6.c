/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* The array has alignment 4 but is at byte 1 of the packed struct, so
   the struct must be passed in memory.  */

struct B { int b : 5; };
struct __attribute__((packed)) S { char a; struct B x[2]; };

char
f (struct S s)
{
  return s.a;
}

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi" } } */
