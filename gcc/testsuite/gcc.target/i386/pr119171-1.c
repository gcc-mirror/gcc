/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

extern long a1, a2, a3, a4;
extern void foo (void *, void *, void *, void *);
void
bar (void *rdi, void *rsi, void *rdx, void *rcx)
{
  asm ("" : "=D"(a1) : "D"(0));
  asm ("" : "=S"(a2) : "S"(0));
  asm ("" : "=d"(a3) : "d"(0));
  asm ("" : "=c"(a4) : "c"(0));
  foo (rdi, rsi, rdx, rcx);
}
