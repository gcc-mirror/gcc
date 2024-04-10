/* { dg-do compile } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -fomit-frame-pointer" } */

extern int bar (int) __attribute__ ((no_caller_saved_registers))
#ifndef __x86_64__
__attribute__ ((regparm(3)))
#endif
;

__attribute__ ((no_callee_saved_registers))
void
foo (void *frame)
{
  int a,b,c,d,e,f,i;
  a = bar (5);
  b = bar (a);
  c = bar (b);
  d = bar (c);
  e = bar (d);
  f = bar (e);
  for (i = 1; i < 10; i++)
  {
    a += bar (a + i) + bar (b + i) +
	 bar (c + i) + bar (d + i) +
	 bar (e + i) + bar (f + i);
  }
}

/* { dg-final { scan-assembler-not "push" } } */
/* { dg-final { scan-assembler-not "pop" } } */
