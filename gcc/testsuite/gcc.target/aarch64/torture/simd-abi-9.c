/* { dg-do compile } */
/* { dg-options "-fshrink-wrap -ffat-lto-objects" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { check-function-bodies "**" "" } } */

int callee (void);

/*
** caller:	{ target lp64 }
**	ldr	(w[0-9]+), \[x0\]
**	cbn?z	\1, [^\n]*
**	...
**	ret
*/
int __attribute__ ((aarch64_vector_pcs))
caller (int *x)
{
  if (*x)
    return callee () + 1;
  else
    return 0;
}

/* { dg-final { scan-assembler {\sstp\tq8, q9} } } */
/* { dg-final { scan-assembler {\sstp\tq10, q11} } } */
/* { dg-final { scan-assembler {\sstp\tq12, q13} } } */
/* { dg-final { scan-assembler {\sstp\tq14, q15} } } */
/* { dg-final { scan-assembler {\sstp\tq16, q17} } } */
/* { dg-final { scan-assembler {\sstp\tq18, q19} } } */
/* { dg-final { scan-assembler {\sstp\tq20, q21} } } */
/* { dg-final { scan-assembler {\sstp\tq22, q23} } } */
/* { dg-final { scan-assembler {\sldp\tq8, q9} } } */
/* { dg-final { scan-assembler {\sldp\tq10, q11} } } */
/* { dg-final { scan-assembler {\sldp\tq12, q13} } } */
/* { dg-final { scan-assembler {\sldp\tq14, q15} } } */
/* { dg-final { scan-assembler {\sldp\tq16, q17} } } */
/* { dg-final { scan-assembler {\sldp\tq18, q19} } } */
/* { dg-final { scan-assembler {\sldp\tq20, q21} } } */
/* { dg-final { scan-assembler {\sldp\tq22, q23} } } */

/* { dg-final { scan-assembler-not {\tstp\tq[0-7],} } } */
/* { dg-final { scan-assembler-not {\tldp\tq[0-7],} } } */
/* { dg-final { scan-assembler-not {\tstp\tq2[4-9],} } } */
/* { dg-final { scan-assembler-not {\tldp\tq2[4-9],} } } */
/* { dg-final { scan-assembler-not {\tstp\td} } } */
/* { dg-final { scan-assembler-not {\tldp\td} } } */
/* { dg-final { scan-assembler-not {\tstr\tq} } } */
/* { dg-final { scan-assembler-not {\tldr\tq} } } */
