/* Verify t1 is saved before use.  */
/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */
void __attribute__ ((interrupt))
foo2 (void)
{
  char array[4096];
  extern volatile int INTERRUPT_FLAG;
  INTERRUPT_FLAG = 0;

  extern volatile int COUNTER;
#ifdef __riscv_atomic
  __atomic_fetch_add (&COUNTER, 1, __ATOMIC_RELAXED);
#else
  COUNTER++;
#endif
}
/* { dg-final { scan-assembler "s\[wd\]\tt1" } } */
