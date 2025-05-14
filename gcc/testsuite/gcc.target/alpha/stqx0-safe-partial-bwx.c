/* { dg-do compile } */
/* { dg-options "-mbwx -msafe-partial" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "stqx0.c"

/* Expect assembly such as:

	stb $31,0($16)
	stb $31,1($16)
	stb $31,2($16)
	stb $31,3($16)
	stb $31,4($16)
	stb $31,5($16)
	stb $31,6($16)
	stb $31,7($16)

   without any LDQ_U or STQ_U instructions.  */

/* { dg-final { scan-assembler-times "\\sstb\\s" 8 } } */
/* { dg-final { scan-assembler-not "\\s(?:ldq_u|stq_u)\\s" } } */
