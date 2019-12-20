/* PR target/92841 */
/* { dg-do compile { target { { { *-*-linux* } && lp64 } && fstack_protector } } } */
/* { dg-options "-O2 -fpic -fstack-protector-strong -masm=att" } */
/* { dg-final { scan-assembler "leaq\tbuf2\\\(%rip\\\)," } } */

static char buf2[64];
void bar (char *, char *);

void
foo ()
{
  char buf[64];
  char *p = buf2;
  asm ("" : "+a" (p));
  char *q = buf;
  asm ("" : "+r" (q));
  bar (p, q);
}
