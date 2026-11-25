/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2" } */

/* The member s has alignment 4 but is at byte 1 of the packed struct, so
   the psABI requires the whole object to be passed in memory.  */

struct B { int b : 5; };
struct __attribute__((packed)) S { char a; struct B s; };

char f (struct S s) { return s.a; } /* { dg-message "the ABI of passing structures and unions with unaligned members has changed in GCC 17.1" } */

/* { dg-final { scan-assembler-not "%dil|%edi|%rdi" } } */
