/* { dg-do compile } */

NOMIPS16 int NoBarrier_AtomicIncrement(volatile int* ptr, int increment) {
  int temp, temp2;
  __asm__ __volatile__(".set push\n"
                       ".set noreorder\n"
                       "1:\n"
                       "ll %0, 0(%3)\n"
                       "addu %1, %0, %2\n"
                       "sc %1, 0(%3)\n"
                       "beqz %1, 1b\n"
                       "nop\n"
                       "addu %1, %0, %2\n"
                       ".set pop\n"
                       : "=&r" (temp), "=&r" (temp2)
                       : "Ir" (increment), "r" (ptr)
                       : "memory");

  return temp2;
}
