/* { dg-do compile } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

#include <stddef.h>

void *
memcpy8 (void *to, const void *from, size_t size)
{
  unsigned char s8 = size;
  return __builtin_memcpy (to, from, s8);
}

/* Expect assembly like:

	movl 4(%ap),%r6
	movzbl 12(%ap),%r7
	movl 8(%ap),%r8
	movc3 %r7,(%r8),(%r6)
	movl %r6,%r0

 */

/* { dg-final { scan-assembler "\tmovc3 " } } */
