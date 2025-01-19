/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef struct { int v __attribute__ ((packed)); } intx;

void
stlx0 (intx *p)
{
  p->v = 0;
}

/* Expect assembly such as:

	ldq_u $2,3($16)
	ldq_u $1,0($16)
	msklh $2,$16,$2
	stq_u $2,3($16)
	mskll $1,$16,$1
	stq_u $1,0($16)

   without any INSLH, INSLL, or BIS instructions.  */

/* { dg-final { scan-assembler-times "\\sldq_u\\s" 2 } } */
/* { dg-final { scan-assembler-times "\\smsklh\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\smskll\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\s(?:bis|inslh|insll)\\s" } } */
