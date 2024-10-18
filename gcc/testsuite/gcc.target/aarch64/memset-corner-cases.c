/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target lp64 } */

#include <stdint.h>

/* One byte variable set should be scalar
**set1byte:
**	strb	w1, \[x0\]
**	ret
*/
void __attribute__((__noinline__))
set1byte (int64_t *src, char c)
{
  __builtin_memset (src, c, 1);
}

/* Special cases for setting 0.  */
/* 1-byte should be STRB with wzr
**set0byte:
**	strb	wzr, \[x0\]
**	ret
*/
void __attribute__((__noinline__))
set0byte (int64_t *src)
{
  __builtin_memset (src, 0, 1);
}

/* 35bytes would become 4 scalar instructions.  So favour NEON.
**set0neon:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	stp	q\1, q\1, \[x0\]
**	str	wzr, \[x0, 31\]
**	ret
*/
void __attribute__((__noinline__))
set0neon (int64_t *src)
{
  __builtin_memset (src, 0, 35);
}

/* 36bytes should be scalar however.
**set0scalar:
**	stp	xzr, xzr, \[x0\]
**	stp	xzr, xzr, \[x0, 16\]
**	str	wzr, \[x0, 32\]
**	ret
*/
void __attribute__((__noinline__))
set0scalar (int64_t *src)
{
  __builtin_memset (src, 0, 36);
}


/* 256-bytes expanded
**set256byte:
**	dup	v([0-9]+).16b, w1
**	stp	q\1, q\1, \[x0\]
**	stp	q\1, q\1, \[x0, 32\]
**	stp	q\1, q\1, \[x0, 64\]
**	stp	q\1, q\1, \[x0, 96\]
**	stp	q\1, q\1, \[x0, 128\]
**	stp	q\1, q\1, \[x0, 160\]
**	stp	q\1, q\1, \[x0, 192\]
**	stp	q\1, q\1, \[x0, 224\]
**	ret
*/
void __attribute__((__noinline__))
set256byte (int64_t *src, char c)
{
  __builtin_memset (src, c, 256);
}

/* More than 256 bytes goes to memset
**set257byte:
**	mov	x2, 257
**	mov	w1, 99
**	b	memset
*/
void __attribute__((__noinline__))
set257byte (int64_t *src)
{
  __builtin_memset (src, 'c', 257);
}

/* { dg-final { check-function-bodies "**" "" "" } } */
