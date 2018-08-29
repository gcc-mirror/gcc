/* Verify that arg regs used as temporaries get saved.  */
/* { dg-do compile } */
/* { dg-options "-O" } */
void __attribute__ ((interrupt))
foo2 (void)
{
  extern volatile int INTERRUPT_FLAG;
  INTERRUPT_FLAG = 0;

  extern volatile int COUNTER;
#ifdef __riscv_atomic
  __atomic_fetch_add (&COUNTER, 1, __ATOMIC_RELAXED);
#else
  COUNTER++;
#endif
}
/* { dg-final { scan-assembler-times "s\[wd\]\ta\[0-7\],\[0-9\]+\\(sp\\)" 2 } } */
