/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapx-features=egpr,push2pop2 -mgeneral-regs-only -mno-cld -mno-push-args -maccumulate-outgoing-args" } */

extern void foo (void *) __attribute__ ((interrupt));

extern int bar (int);

void foo (void *frame)
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

/* { dg-final { scan-assembler-times "pushq" 31 } } */
/* { dg-final { scan-assembler-times "popq" 31 } } */
/* { dg-final { scan-assembler-not "push2\[\\t \]+" } } */
/* { dg-final { scan-assembler-not "pop2\[\\t \]+" } } */
