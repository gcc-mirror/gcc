/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2" } */

/* The member is misaligned, but the struct was already passed in memory
   because of its int member, so the ABI did not change and there must be
   no note.  */

struct __attribute__((packed)) S { char a; struct { int x; } s; };

char f (struct S s) { return s.a; }

/* { dg-bogus "the ABI of passing" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler-not "%dil|%edi|%rdi" } } */
