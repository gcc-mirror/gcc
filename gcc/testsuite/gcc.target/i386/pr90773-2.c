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
/* PIC gets one extra match in get_pc_thunk, and two extra matches to load
   dst's and src's values after loading their addresses from the GOT.  */
/* { dg-final { scan-assembler-times "movl\[\\t \]+\\(%\[\^,\]+\\)," 1 { target { ia32 && nonpic } } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+\\(%\[\^,\]+\\)," 4 { target { ia32 && { ! nonpic } } } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+4\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+8\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+12\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]+15\\(%\[\^,\]+\\)," 1 { target ia32 } } } */
