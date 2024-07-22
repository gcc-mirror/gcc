/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection -fno-asynchronous-unwind-tables -fno-unwind-tables -fno-stack-protector --param stack-clash-protection-guard-size=16" } */
/* { dg-skip-if "" { *-*-* } { "-g"} } */
/* { dg-require-effective-target supports_stack_clash_protection } */

typedef unsigned __attribute__((mode(DI))) uint64_t;

extern void arf (uint64_t *, uint64_t *);
void
frob ()
{
  uint64_t num[10000];
  uint64_t den[10000];
  arf (den, num);
}

/* This verifies that the scheduler did not break the dependencies
   by adjusting the offsets within the probe and that the scheduler
   did not reorder around the stack probes.  */
/* { dg-final { scan-assembler-times "li\\tt0,65536" 1 } } */
/* { dg-final { scan-assembler-times "sub\\tsp,sp,t0\\n\\tsd\\tzero,1024\\(sp\\)" 2 } } */
/* There is some residual allocation, but we don't care about that. Only that it's not probed.  */
/* { dg-final { scan-assembler-times "sd\\tzero," 2 } } */
