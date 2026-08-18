/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -march=x86-64 -msse4 -std=c++17" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**_Z4funcDv2_iS_PDv2_x:
**.LFB0:
**	.cfi_startproc
**	pminsd	%xmm1, %xmm0
**	pmovsxdq	%xmm0, %xmm1
**	movaps	%xmm1, \(%[er]di\)
**	ret
**...
*/

typedef int  v2si __attribute__((vector_size (8)));
typedef long long v2di __attribute__((vector_size (16)));

v2si
func (v2si a, v2si b, v2di *p)
{
  v2di x = __builtin_convertvector (a, v2di);
  v2di y = __builtin_convertvector (b, v2di);
  v2di z = x < y ? x : y;
  *p = z;
  return __builtin_convertvector (z, v2si);
}
