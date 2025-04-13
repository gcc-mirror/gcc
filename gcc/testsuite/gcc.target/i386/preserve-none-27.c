/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fno-pic -mtune=generic -msse2 -mno-apxf -mtune-ctrl=prologue_using_move,epilogue_using_move" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB[0-9]+:
**	.cfi_startproc
**...
**	movl	%edi, %r12d
**	movl	%esi, %r13d
**	movl	%edx, %r14d
**	pushq	\$-559038737
**...
**	movl	%ecx, %r15d
**	movl	%r9d, %esi
**	movl	%r8d, %edi
**	xorl	%eax, %eax
**...
**	call	continuation
**...
*/

extern void continuation (int, int, int, int, int, int, ...)
  __attribute__ ((preserve_none));

__attribute__ ((no_callee_saved_registers))
void
entry (int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
  continuation (arg1, arg2, arg3, arg4, arg5, arg6, 0xdeadbeef);
}
