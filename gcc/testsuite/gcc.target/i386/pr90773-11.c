/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

extern char *dst;

void
foo (int c)
{
  __builtin_memset (dst, c, 6);
}

/* { dg-final { scan-assembler-times "movl\[\\t \]+.+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movw\[\\t \]+.+, 4\\(%\[\^,\]+\\)" 1 } } */
