/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mmove-max=512 -mtune=generic -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**gcc_memmove_zmm:
**.LFB0:
**	.cfi_startproc
**	vmovdqu64	\(%(e|r)si\), %zmm7
**	vmovdqu64	64\(%(e|r)si\), %zmm6
**	vmovdqu64	128\(%(e|r)si\), %zmm5
**	vmovdqu64	192\(%(e|r)si\), %zmm4
**	vmovdqu64	256\(%(e|r)si\), %zmm3
**	vmovdqu64	320\(%(e|r)si\), %zmm2
**	vmovdqu64	384\(%(e|r)si\), %zmm1
**	vmovdqu64	448\(%(e|r)si\), %zmm0
**	vmovdqu64	%zmm7, \(%(e|r)di\)
**	vmovdqu64	%zmm6, 64\(%(e|r)di\)
**	vmovdqu64	%zmm5, 128\(%(e|r)di\)
**	vmovdqu64	%zmm4, 192\(%(e|r)di\)
**	vmovdqu64	%zmm3, 256\(%(e|r)di\)
**	vmovdqu64	%zmm2, 320\(%(e|r)di\)
**	vmovdqu64	%zmm1, 384\(%(e|r)di\)
**	vmovdqu64	%zmm0, 448\(%(e|r)di\)
**	vzeroupper
**	ret
**	.cfi_endproc
**...
*/

#define gcc_memmove gcc_memmove_zmm
#include "builtin-memmove-11a.c"
