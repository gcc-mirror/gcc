/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-require-effective-target lp64 } */

#include <stdint.h>

/* 127 bytes should use libcall for size.
**set127byte:
**	mov	x2, 127
**	b	memset
*/
void __attribute__((__noinline__))
set127byte (int64_t *src, int c)
{
  __builtin_memset (src, c, 127);
}

/* 128 bytes should use libcall for size.
**set128byte:
**	mov	x2, 128
**	b	memset
*/
void __attribute__((__noinline__))
set128byte (int64_t *src, int c)
{
  __builtin_memset (src, c, 128);
}

/* { dg-final { check-function-bodies "**" "" "" } } */

