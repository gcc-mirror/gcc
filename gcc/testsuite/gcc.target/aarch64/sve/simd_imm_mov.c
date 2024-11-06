/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

typedef short v8hi __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
typedef long v2di __attribute__((vector_size(16)));

/*
** t1:
**	mov	z0.s, #?4092
**	ret
*/
v4si t1 ()
{
  return (v4si) { 0xffc, 0xffc, 0xffc, 0xffc };
}

/*
** t2:
**	mov	z0.h, #?510
**	ret
*/
v8hi t2 ()
{
  return (v8hi) { 510, 510, 510, 510, 510, 510, 510, 510 };
}

/*
** t3:
**	mov	z0.d, #?1
**	ret
*/
v2di t3 ()
{
  return (v2di) { 1, 1 };
}
