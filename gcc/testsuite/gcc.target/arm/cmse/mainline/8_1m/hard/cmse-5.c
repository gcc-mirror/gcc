/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "Skip these if testing single precision" {*-*-*} {"-mfpu=*-sp-*"} {""} } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "../../../cmse-5.x"

/*
** __acle_se_foo:
**	vstr	FPCXTNS, \[sp, #-4\]!
** ...
**	vscclrm	\{s1-s15, VPR\}
**	clrm	\{r0, r1, r2, r3, ip, APSR\}
**	vldr	FPCXTNS, \[sp\], #4
**	bxns	lr
*/
