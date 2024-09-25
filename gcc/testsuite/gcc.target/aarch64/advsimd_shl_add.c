/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef __INT64_TYPE__ __attribute__ ((vector_size (16))) v2di;
typedef int __attribute__ ((vector_size (16))) v4si;
typedef short __attribute__ ((vector_size (16))) v8hi;
typedef char __attribute__ ((vector_size (16))) v16qi;
typedef short __attribute__ ((vector_size (8))) v4hi;
typedef char __attribute__ ((vector_size (8))) v8qi;

#define FUNC(S) \
S               \
foo_##S (S a)   \
{ return a << 1; }

/*
** foo_v2di:
**      add	v0.2d, v0.2d, v0.2d
**      ret
*/

FUNC (v2di)

/*
** foo_v4si:
**      add	v0.4s, v0.4s, v0.4s
**      ret
*/

FUNC (v4si)

/*
** foo_v8hi:
**      add	v0.8h, v0.8h, v0.8h
**      ret
*/

FUNC (v8hi)

/*
** foo_v16qi:
**      add	v0.16b, v0.16b, v0.16b
**      ret
*/

FUNC (v16qi)

/*
** foo_v4hi:
**      add	v0.4h, v0.4h, v0.4h
**      ret
*/

FUNC (v4hi)

/*
** foo_v8qi:
**      add	v0.8b, v0.8b, v0.8b
**      ret
*/

FUNC (v8qi)

