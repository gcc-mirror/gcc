/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue" } */

/* Avoid inserting sub between test-je-jle to change EFlags, lea should be used here
        xorl    %eax, %eax
        testl   %edi, %edi
        je      .L11
        sub     $16, %rsp  ------> leaq    -16(%rsp), %rsp
        movq    %r13, 8(%rsp)
        movl    $1, %r13d
        jle     .L4
*/
int foo (int num)
{
  if (!num)
    return 0;

  register int r13 __asm ("r13") = 1;

  for ( int i = 0; i < num; i++)
    {
      register int r12 __asm ("r12") = 1;
      asm volatile ("" : "+r" (r12), "+r" (r13));
    }

  return 1;
}
/* { dg-final { scan-rtl-dump "The components we wrap separately are \\\[sep 40\\\]" "pro_and_epilogue" } } */
/* { dg-final { scan-assembler "lea(l|q).*(%rsp)" } } */
