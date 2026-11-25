/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -Wno-psabi" } */

/* The aligned attribute of the typedef lowers the alignment of the type
   variant, so the member is at byte 1, but the natural alignment of its
   type is 4.  The struct must be passed in memory, like a struct with a
   scalar member of such a type.  */

struct B { int b : 5; };
typedef struct B __attribute__((aligned(1))) B_a1;
struct S { char a; B_a1 s; };

char f (struct S s) { return s.a; }

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi" } } */
