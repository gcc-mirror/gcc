/* { dg-do compile } */
/* { dg-options "-mno-safe-partial" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef struct { long v __attribute__ ((packed)); } longx;

void
stqx0 (longx *p)
{
  p->v = 0;
}

/* Expect assembly such as:

	ldq_u $2,7($16)
	ldq_u $1,0($16)
	mskqh $2,$16,$2
	stq_u $2,7($16)
	mskql $1,$16,$1
	stq_u $1,0($16)

   without any INSQH, INSQL, or BIS instructions.  */

/* { dg-final { scan-assembler-times "\\sldq_u\\s" 2 } } */
/* { dg-final { scan-assembler-times "\\smskqh\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\smskql\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\s(?:bis|insqh|insql)\\s" } } */
