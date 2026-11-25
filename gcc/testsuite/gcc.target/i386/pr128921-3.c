/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { { ! ia32 } && bitint } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* unsigned _BitInt(33) has alignment 8 but is at byte 1 of the packed
   struct, so the object must be passed in memory.  */

typedef struct __attribute__((packed)) {
  char a;
  unsigned _BitInt(33) b;
} S;

char
f (S s)
{
  return s.a;
}

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi" } } */
