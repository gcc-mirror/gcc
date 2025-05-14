/* { dg-do compile } */
/* { dg-options "-mno-bwx -msafe-partial" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "stqx0.c"

/* Expect assembly such as:

	lda $2,7($16)
	bic $2,7,$2
$L2:
	ldq_l $1,0($2)
	mskqh $1,$16,$1
	stq_c $1,0($2)
	beq $1,$L2
	bic $16,7,$2
$L3:
	ldq_l $1,0($2)
	mskql $1,$16,$1
	stq_c $1,0($2)
	beq $1,$L3

   without any INSLH, INSLL, BIS, LDQ_U, or STQ_U instructions.  */

/* { dg-final { scan-assembler-times "\\sldq_l\\s" 2 } } */
/* { dg-final { scan-assembler-times "\\smskqh\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\smskql\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_c\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\s(?:bis|insqh|insql|ldq_u|stq_u)\\s" } } */
