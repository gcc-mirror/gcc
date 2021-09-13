/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */
/* { dg-additional-options "-mno-avx -msse2" { target { ! ia32 } } } */
/* { dg-additional-options "-mno-sse" { target ia32 } } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 19);
}

/* { dg-final { scan-assembler-times "movdqu\[\\t \]+\\(%\[\^,\]+\\)," 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+15\\(%\[\^,\]+\\)," 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+4\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+8\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+12\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+15\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
