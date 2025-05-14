/* { dg-do compile } */
/* { dg-options "-mbwx -msafe-partial" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "memclr-a2-o1-c9-ptr.c"

/* Expect assembly such as:

	stb $31,1($16)
	stw $31,2($16)
	stw $31,4($16)
	stw $31,6($16)
	stw $31,8($16)

   that is with a byte store at offset 1, followed by word stores at
   offsets 2, 4, 6, and 8.  */

/* { dg-final { scan-assembler-times "\\sstb\\s\\\$31,1\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstw\\s\\\$31,2\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstw\\s\\\$31,4\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstw\\s\\\$31,6\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstw\\s\\\$31,8\\\(\\\$16\\\)\\s" 1 } } */
