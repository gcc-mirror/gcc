/* { dg-do compile } */
/* { dg-options "-mno-bwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef struct { short v __attribute__ ((packed)); } shortx;

void
stwx0 (shortx *p)
{
  p->v = 0;
}

/* Expect assembly such as:

	ldq_u $2,1($16)
	ldq_u $1,0($16)
	mskwh $2,$16,$2
	stq_u $2,1($16)
	mskwl $1,$16,$1
	stq_u $1,0($16)

   without any INSWH, INSWL, or BIS instructions.  */

/* { dg-final { scan-assembler-times "\\sldq_u\\s" 2 } } */
/* { dg-final { scan-assembler-times "\\smskwh\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\smskwl\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\s(?:bis|inswh|inswl)\\s" } } */
