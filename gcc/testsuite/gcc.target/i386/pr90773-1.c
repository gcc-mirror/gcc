/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 15);
}

/* { dg-final { scan-assembler-times "movq\[\\t \]+\\(%\[\^,\]+\\)," 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movq\[\\t \]+7\\(%\[\^,\]+\\)," 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+4\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+8\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+11\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
