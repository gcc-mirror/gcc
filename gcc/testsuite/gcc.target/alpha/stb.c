/* { dg-do compile } */
/* { dg-options "-mno-bwx -mno-safe-bwa" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void
stb (char *p, char v)
{
  *p = v;
}

/* Expect assembly such as:

	insbl $17,$16,$17
	ldq_u $1,0($16)
	mskbl $1,$16,$1
	bis $17,$1,$17
	stq_u $17,0($16)

   without address masking.  */

/* { dg-final { scan-assembler-times "\\sldq_u\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sinsbl\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\smskbl\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\sbic\\s\\\$\[0-9\]+,7,\\\$\[0-9\]+\\s" } } */
