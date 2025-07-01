/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -fasynchronous-unwind-tables -fdwarf2-cfi-asm" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	subq	\$16, %rsp
**	.cfi_def_cfa_offset 24
**	pxor	%xmm0, %xmm0
**	movaps	%xmm0, -120\(%rsp\)
**	movaps	%xmm0, -104\(%rsp\)
**	movaps	%xmm0, -88\(%rsp\)
**	movaps	%xmm0, -72\(%rsp\)
**	movaps	%xmm0, -56\(%rsp\)
**	movaps	%xmm0, -40\(%rsp\)
**	movaps	%xmm0, -24\(%rsp\)
**	movaps	%xmm0, -8\(%rsp\)
**	xorl	%eax, %eax
**	addq	\$16, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**...
*/

extern int scanf (const char *, ...);
extern void *memset (void *, int, __SIZE_TYPE__);

int
foo (void)
{
  char buf[128];

#if USE_SCANF
  if (scanf("%s", buf) != 1)
    return 42;
#endif

  memset (buf,0, sizeof (buf));
  asm volatile("": : :"memory");
  return 0;
}

/* { dg-final { scan-assembler-not "rep stos" } } */
