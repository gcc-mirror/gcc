/* { dg-do compile } */
/* { dg-options "-mno-bwx -msafe-bwa" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void
stw (short *p, short v)
{
  *p = v;
}

/* Expect assembly such as:

	bic $16,7,$2
	inswl $17,$16,$17
$L2:
	ldq_l $1,0($2)
	mskwl $1,$16,$1
	bis $17,$1,$1
	stq_c $1,0($2)
	beq $1,$L2

   with address masking.  */

/* { dg-final { scan-assembler-times "\\sldq_l\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_c\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sinswl\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\smskwl\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sbic\\s\\\$\[0-9\]+,7,\\\$\[0-9\]+\\s" 1 } } */
