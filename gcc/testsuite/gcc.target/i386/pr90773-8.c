/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 0, 5);
}

/* { dg-final { scan-assembler-times "movl\[\\t \]+.+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movb\[\\t \]+.+, 4\\(%\[\^,\]+\\)" 1 } } */
