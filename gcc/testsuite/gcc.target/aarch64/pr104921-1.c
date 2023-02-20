/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=armv8.2-a+bf16 -std=gnu99 -save-temps" }  */
/* { dg-final { check-function-bodies "**" "" } } */

#include "pr104921.x"

/*
**foo:
**	mov	v([0-9]|1[0-5])\.8b, v16\.8b
**	bfmlalb	v0\.4s, v1\.8h, v([0-9]|1[0-5])\.h\[0\]
**	ret
*/
