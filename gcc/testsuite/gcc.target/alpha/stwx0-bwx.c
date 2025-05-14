/* { dg-do compile } */
/* { dg-options "-mbwx -mno-safe-partial" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "stwx0.c"

/* Expect assembly such as:

	stb $31,0($16)
	stb $31,1($16)

   without any LDQ_U or STQ_U instructions.  */

/* { dg-final { scan-assembler-times "\\sstb\\s\\\$31," 2 } } */
/* { dg-final { scan-assembler-not "\\s(?:ldq_u|stq_u)\\s" } } */
