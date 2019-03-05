/* { dg-do compile } */
/* { dg-options "-fno-sched-last-insn-heuristic -fno-dce -march=z196 -O2" } */

void *a, *b;

void c(void)
{
  __builtin_memcpy(a, b, -1);  /* { dg-warning "exceeds maximum object size" } */
}
