/* { dg-do compile target { { rv32-*-*}}} */
/* { dg-options "-march=rv32gc_zicbop -mabi=ilp32" } */

void foo (char *p)
{
  __builtin_prefetch (p, 0, 0);
  __builtin_prefetch (p, 0, 1);
  __builtin_prefetch (p, 0, 2);
  __builtin_prefetch (p, 0, 3);
  __builtin_prefetch (p, 1, 0);
  __builtin_prefetch (p, 1, 1);
  __builtin_prefetch (p, 1, 2);
  __builtin_prefetch (p, 1, 3);
}

int foo1()
{
  return __builtin_riscv_zicbop_cbo_prefetchi(1);
}

/* { dg-final { scan-assembler-times "prefetch.i" 1 } } */
/* { dg-final { scan-assembler-times "prefetch.r" 4 } } */
/* { dg-final { scan-assembler-times "prefetch.w" 4 } } */ 
