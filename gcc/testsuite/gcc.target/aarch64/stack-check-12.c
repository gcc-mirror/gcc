/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16 -fno-asynchronous-unwind-tables -fno-unwind-tables" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

extern void arf (unsigned long int *, unsigned long int *);
void
frob ()
{
  unsigned long int num[10000];
  unsigned long int den[10000];
  arf (den, num);
}

/* This verifies that the scheduler did not break the dependencies
   by adjusting the offsets within the probe and that the scheduler
   did not reorder around the stack probes.  */
/* { dg-final { scan-assembler-times {sub\tsp, sp, #65536\n\tstr\txzr, \[sp, 1024\]} 2 } } */
/* There is some residual allocation, but we don't care about that. Only that it's not probed.  */
/* { dg-final { scan-assembler-times {str\txzr, } 2 } } */



