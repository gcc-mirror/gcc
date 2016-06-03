/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -miamcu -maccumulate-outgoing-args" } */

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
    a += bar (a + i) + bar (b + i) +
	 bar (c + i) + bar (d + i) +
	 bar (e + i) + bar (f+i);
}

/* { dg-final { scan-assembler-times "pushl\[\\t \]*%eax" 1 } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%ebx" 1 } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%ecx" 1 } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%edx" 1 } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%edi" 1 } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%esi" 1 } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%ebp" 1 } } */
/* { dg-final { scan-assembler-times "popl\[\\t \]*%eax" 1 } } */
/* { dg-final { scan-assembler-times "popl\[\\t \]*%ecx" 1 } } */
/* { dg-final { scan-assembler-times "popl\[\\t \]*%edx" 1 } } */
/* { dg-final { scan-assembler-times "popl\[\\t \]*%edi" 1 } } */
/* { dg-final { scan-assembler-times "popl\[\\t \]*%esi" 1 } } */
/* { dg-final { scan-assembler-times "popl\[\\t \]*%ebp" 1 } } */
/* { dg-final { scan-assembler-times "iret" 1 } } */
/* { dg-final { scan-assembler-times "\tcld" 1 } } */
