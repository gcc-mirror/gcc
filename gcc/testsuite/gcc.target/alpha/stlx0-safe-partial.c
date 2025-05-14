/* { dg-do compile } */
/* { dg-options "-mno-bwx -msafe-partial" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "stlx0.c"

/* Expect assembly such as:

	lda $2,3($16)
	bic $2,7,$2
$L2:
	ldq_l $1,0($2)
	msklh $1,$16,$1
	stq_c $1,0($2)
	beq $1,$L2
	bic $16,7,$2
$L3:
	ldq_l $1,0($2)
	mskll $1,$16,$1
	stq_c $1,0($2)
	beq $1,$L3

   without any INSLH, INSLL, BIS, LDQ_U, or STQ_U instructions.  */

/* { dg-final { scan-assembler-times "\\sldq_l\\s" 2 } } */
/* { dg-final { scan-assembler-times "\\smsklh\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\smskll\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_c\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\s(?:bis|inslh|insll|ldq_u|stq_u)\\s" } } */
