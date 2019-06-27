/* { dg-do run } */
/* { dg-options "-O2" }  */
/* { dg-skip-if "" { ! { arm_thumb1 } } } */

int __attribute__((noclone, noinline))
foo (int a, long long b) {
  /* Used for enforcing registers stacking.  */
  asm volatile ("" : : : "r0", "r1", "r2", "r3",
			 "r8", "r9", "r10", "r11", "r12");
  return (int) b;
}

int main ()
{
  if (foo (1, 0x1000000000000003LL) != 3)
    __builtin_abort ();
  __builtin_exit (0);
}
