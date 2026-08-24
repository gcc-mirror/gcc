/* PR target/126231  */
/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

struct foo
{
  int i;
  int j;
  int k;
};

struct bar1
{
  double x;
  double y;
  double z;
};

struct bar2
{
  float x;
  float y;
  float z;
};

/*
**func1:
**.LFB[0-9]+:
**	.cfi_startproc
**	cvttsd2sil	16\(%rdi\), %ecx
**	movupd	\(%rdi\), %xmm0
**	cvttpd2dq	%xmm0, %xmm1
**	movq	%xmm1, %rax
**	movl	%ecx, %ecx
**	movq	%rcx, %rdx
**	ret
**	.cfi_endproc
**...
*/

struct foo
func1 (struct bar1 *x)
{ 
  return (struct foo) { x->x, x->y, x->z };
}

/*
**func2:
**.LFB[0-9]+:
**	.cfi_startproc
**	cvttss2sil	8\(%rdi\), %eax
**	movq	\(%rdi\), %xmm0
**	cvttps2dq	%xmm0, %xmm0
**	movq	%xmm0, %rsi
**	movl	%eax, %eax
**	movq	%rax, %rdi
**	movq	%rsi, %rax
**	movq	%rdi, %rdx
**	ret
**	.cfi_endproc
**...
*/

struct foo
func2 (struct bar2 *x)
{ 
  return (struct foo) { x->x, x->y, x->z };
}
