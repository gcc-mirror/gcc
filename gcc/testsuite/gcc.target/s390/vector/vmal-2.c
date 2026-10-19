/* { dg-do compile } */
/* { dg-options "-O2 -march=z13" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Tests for VMAL with a constant multiplicand.  If the constant is small, then
   we compete with a synthetic multiplication which in turn means those tests
   are subject to costing and therefore may be a bit fragile.  */

typedef signed char v16qi __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef long v2di __attribute__ ((vector_size (16)));
typedef unsigned long uv2di __attribute__ ((vector_size (16)));
typedef __int128 v1ti __attribute__ ((vector_size (16)));

#define CST_VEC (uv2di){0xBADC0FFEE0DDF00D, 0xBADC0FFEE0DDF00D}

/*
** test_v16qi_plus_vrepi:
**	veslb	(%v[0-9]+),%v26,5
**	vsb	(%v[0-9]+),\1,%v26
**	veslb	(%v[0-9]+),\2,2
**	vsb	(%v[0-9]+),\3,%v26
**	vab	%v24,\4,%v24
**	br	%r14
*/

v16qi test_v16qi_plus_vrepi (v16qi a, v16qi b)
{
  return a + b * 123;
}

/*
** test_v16qi_plus_constpool:
**	larl	.*
**	vl	(%v[0-9]+),.*
**	vmalb	%v24,%v26,\1,%v24
**	br	%r14
*/

v16qi test_v16qi_plus_constpool (v16qi a, v16qi b)
{
  return a + b * (v16qi)CST_VEC;
}

/*
** test_v8hi_plus_vrepi:
**	vrepi	(%v[0-9]+),1234,1
**	vmalhw	%v24,%v26,\1,%v24
**	br	%r14
*/

v8hi test_v8hi_plus_vrepi (v8hi a, v8hi b)
{
  return a + b * 1234;
}

/*
** test_v8hi_plus_constpool:
**	larl	.*
**	vl	(%v[0-9]+),.*
**	vmalhw	%v24,%v26,\1,%v24
**	br	%r14
*/

v8hi test_v8hi_plus_constpool (v8hi a, v8hi b)
{
  return a + b * (v8hi)CST_VEC;
}

/*
** test_v4si_plus_vrepi:
**	vrepi	(%v[0-9]+),1234,2
**	vmalf	%v24,%v26,\1,%v24
**	br	%r14
*/

v4si test_v4si_plus_vrepi (v4si a, v4si b)
{
  return a + b * 1234;
}

/*
** test_v4si_plus_constpool:
**	larl	.*
**	vl	(%v[0-9]+),.*
**	vmalf	%v24,%v26,\1,%v24
**	br	%r14
*/

v4si test_v4si_plus_constpool (v4si a, v4si b)
{
  return a + b * (v4si)CST_VEC;
}

/*
** test_v2di_plus_vrepi:
**	vrepi	(%v[0-9]+),1234,3
**	vmalg	%v24,%v26,\1,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
v2di test_v2di_plus_vrepi (v2di a, v2di b)
{
  return a + b * 1234;
}

/*
** test_v2di_plus_constpool:
**	larl	.*
**	vl	(%v[0-9]+),.*
**	vmalg	%v24,%v26,\1,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
v2di test_v2di_plus_constpool (v2di a, v2di b)
{
  return a + b * (v2di)CST_VEC;
}

/*
** test_v1ti_plus_vrepi:
**	vrepi	(%v[0-9]+),-86,0
**	vmalq	%v24,%v26,\1,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
v1ti test_v1ti_plus_vrepi (v1ti a, v1ti b)
{
  return a + b * ((__int128)0xaaaaaaaaaaaaaaaa << 64 | (__int128)0xaaaaaaaaaaaaaaaa);
}

/*
** test_v1ti_plus_constpool:
**	larl	.*
**	vl	(%v[0-9]+),.*
**	vmalq	%v24,%v26,\1,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
v1ti test_v1ti_plus_constpool (v1ti a, v1ti b)
{
  return a + b * ((__int128)0xBADC0FFEE0DDF00D << 64 | (__int128)0xBADC0FFEE0DDF00D);
}

/*
** test_int128_plus_vrepi:
**	(?:vl|vrepi)	.*
**	(?:vl|vrepi)	.*
**	(?:vl|vrepi)	.*
**	vmalq	(%v[0-9]+),.*
**	vst	\1,0\(%r2\),3
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
__int128 test_int128_plus_vrepi (__int128 a, __int128 b)
{
  return a + b * ((__int128)0xaaaaaaaaaaaaaaaa << 64 | (__int128)0xaaaaaaaaaaaaaaaa);
}

/*
** test_int128_plus_constpool:
**	larl	.*
**	vl	.*
**	vl	.*
**	vl	.*
**	vmalq	(%v[0-9]+),.*
**	vst	\1,0\(%r2\),3
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
__int128 test_int128_plus_constpool (__int128 a, __int128 b)
{
  return a + b * ((__int128)0xBADC0FFEE0DDF00D << 64 | (__int128)0xBADC0FFEE0DDF00D);
}
