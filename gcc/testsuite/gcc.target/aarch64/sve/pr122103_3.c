/* { dg-do compile }  */
/* { dg-options "-O3 -fno-math-errno -fno-trapping-math -march=armv9-a" }  */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f:
** 	...
** 	whilelo	p([0-9]+).s, wzr, w[0-9]+
** 	...
** 	ld1w	z[0-9]+.s, p\1/z, \[x[0-9]+, x[0-9]+, lsl 2\]
** 	cmpgt	p\1.s, p\1/z, z[0-9]+.s, z[0-9]+.s
** 	ld1w	z[0-9]+.s, p\1/z, \[x[0-9]+, x[0-9]+, lsl 2\]
** 	fsqrt	z[0-9]+.s, p[0-9]+/m, z[0-9]+.s
** 	st1w	z[0-9]+.s, p\1, \[x[0-9]+, x[0-9]+, lsl 2\]
** 	incw	x[0-9]+
** 	whilelo	p\1.s, w[0-9]+, w[0-9]+
** 	...
*/
void f (float *__restrict c, int *__restrict d, int n)
{
    for (int i = 0; i < n; i++)
    {
      if (d[i] > 1000)
        c[i] = __builtin_sqrtf (c[i]);
    }
}

