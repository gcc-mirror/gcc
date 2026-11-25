/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2" } */

/* The member is aligned to the alignment of its type, so S must still
   be passed in a register and there is no ABI change, hence no note.  */

struct B { int b : 5; };
struct S { char a; struct B s; };

int f (struct S s) { return s.a + s.s.b; }

/* { dg-bogus "the ABI of passing" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler "%\[er\]di" } } */
/* { dg-final { scan-assembler-not "\\(%\[er\]sp\\)" } } */
