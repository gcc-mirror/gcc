/* { dg-do compile } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

#include <stddef.h>

void *
memset8 (void *block, int c, size_t size)
{
  unsigned char s8 = size;
  return __builtin_memset (block, c, s8);
}

/* Expect assembly like:

	movl 4(%ap),%r6
	movzbl 12(%ap),%r7
	movc5 $0,(%ap),8(%ap),%r7,(%r6)
	movl %r6,%r0

 */

/* { dg-final { scan-assembler "\tmovc5 \\\$0,\\\(%ap\\\)," } } */
