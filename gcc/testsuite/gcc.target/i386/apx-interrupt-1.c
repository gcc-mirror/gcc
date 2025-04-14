/* { dg-do compile { target { { ! ia32 } && cfi } } } */
/* { dg-options "-mapx-features=egpr -m64 -O2 -mgeneral-regs-only -mno-cld -mno-push-args -maccumulate-outgoing-args -fomit-frame-pointer" } */

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
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)ax" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)bx" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)cx" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)dx" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)si" 1 } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%rdi" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r8" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r9" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r10" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r11" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r12" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r13" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r14" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r15" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r16" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r17" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r18" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r19" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r20" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r21" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r22" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r23" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r24" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r25" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r26" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r27" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r28" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r29" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r30" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r31" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 145, -16} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 144, -24} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 143, -32} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 142, -40} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 141, -48} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 140, -56} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 139, -64} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 138, -72} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 137, -80} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 136, -88} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 135, -96} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 134, -104} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 133, -112} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 132, -120} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 131, -128} 1 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_offset 130, -136} 1 } } */
/* { dg-final { scan-assembler-times ".cfi_restore" 31 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)ax" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)bx" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)cx" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)dx" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)si" 1 } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%rdi" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r8" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r9" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r10" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r11" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r12" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r13" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r14" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r15" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r16" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r17" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r18" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r19" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r20" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r21" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r22" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r23" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r24" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r25" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r26" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r27" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r28" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r29" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r30" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r31" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "iret" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcld" 1 } } */
