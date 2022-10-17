/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 12, 9);
}

/* { dg-final { scan-assembler-times "movabsq\[\\t \]+\\\$868082074056920076, %r" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+\\\$202116108, \\(%\[\^,\]+\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+\\\$202116108, 4\\(%\[\^,\]+\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movb\[\\t \]+\\\$12, 8\\(%\[\^,\]+\\)" 1 } } */
