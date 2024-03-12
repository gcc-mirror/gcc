/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapxf -m64" } */
/* { dg-final { scan-assembler "r31" } } */
/* { dg-final { scan-assembler "r30" } } */
/* { dg-final { scan-assembler "r29" } } */
/* { dg-final { scan-assembler "r28" } } */
void foo ()
{
  register long a __asm ("r31");
  register int b __asm ("r30");
  register short c __asm ("r29");
  register char d __asm ("r28");
  __asm__ __volatile__ ("mov %0, %%rax" : : "r" (a) : "rax");
  __asm__ __volatile__ ("mov %0, %%eax" : : "r" (b) : "eax");
  __asm__ __volatile__ ("mov %0, %%eax" : : "r" (c) : "eax");
  __asm__ __volatile__ ("mov %0, %%eax" : : "r" (d) : "eax");
}
