/* { dg-do compile } */
/* { dg-options "-O2 -march=z13" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef signed char v16qi __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef long v2di __attribute__ ((vector_size (16)));
typedef __int128 v1ti __attribute__ ((vector_size (16)));

typedef _Float16 v8hf __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));
typedef long double v1tf __attribute__ ((vector_size (16)));

/* INTEGER */

/*
** rev_v16qi_rr:
**	larl	(%r[0-9]+),.L[0-9]+
**	vl	(%v[0-9]+),.L[0-9]+-.L[0-9]+\(\1\),3
**	vperm	%v24,%v24,%v24,\2
**	br	%r14
*/

v16qi
rev_v16qi_rr (v16qi x)
{
  v16qi m = { 15, 14, 13, 12, 11, 10, 9, 8,
               7,  6,  5,  4,  3,  2, 1, 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v16qi_rm:
**	larl	%r[0-9]+,.L[0-9]+
**	vl	.*
**	vl	.*
**	vperm	%v24,.*
**	br	%r14
*/

v16qi
rev_v16qi_rm (v16qi *x)
{
  v16qi m = { 15, 14, 13, 12, 11, 10, 9, 8,
               7,  6,  5,  4,  3,  2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v16qi_rm_z15:
**	vlbrq	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v16qi
rev_v16qi_rm_z15 (v16qi *x)
{
  v16qi m = { 15, 14, 13, 12, 11, 10, 9, 8,
               7,  6,  5,  4,  3,  2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v16qi_mr:
**	larl	(%r[0-9]+),.L[0-9]+
**	vl	(%v[0-9]+),.L[0-9]+-.L[0-9]+\(\1\),3
**	vperm	(%v[0-9]+),%v24,%v24,\2
**	vst	\3,0\(%r2\),3
**	br	%r14
*/

void
rev_v16qi_mr (v16qi *x, v16qi y)
{
  v16qi m = { 15, 14, 13, 12, 11, 10, 9, 8,
               7,  6,  5,  4,  3,  2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v16qi_mr_z15:
**	vstbrq	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v16qi_mr_z15 (v16qi *x, v16qi y)
{
  v16qi m = { 15, 14, 13, 12, 11, 10, 9, 8,
               7,  6,  5,  4,  3,  2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v8hi_rr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	verllg	(%v[0-9]+),\1,32
**	verllf	%v24,\2,16
**	br	%r14
*/

v8hi
rev_v8hi_rr (v8hi x)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v8hi_rm:
**	vl	(%v[0-9]+),0\(%r2\),3
**	vpdi	(%v[0-9]+),\1,\1,4
**	verllg	(%v[0-9]+),\2,32
**	verllf	%v24,\3,16
**	br	%r14
*/

v8hi
rev_v8hi_rm (v8hi *x)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v8hi_rm_z15:
**	vlerh	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v8hi
rev_v8hi_rm_z15 (v8hi *x)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v8hi_mr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	verllg	(%v[0-9]+),\1,32
**	verllf	(%v[0-9]+),\2,16
**	vst	\3,0\(%r2\),3
**	br	%r14
*/

void
rev_v8hi_mr (v8hi *x, v8hi y)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v8hi_mr_z15:
**	vsterh	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v8hi_mr_z15 (v8hi *x, v8hi y)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v4si_rr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	verllg	%v24,\1,32
**	br	%r14
*/

v4si
rev_v4si_rr (v4si x)
{
  v4si m = { 3, 2, 1, 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v4si_rm:
**	vl	(%v[0-9]+),0\(%r2\),3
**	vpdi	(%v[0-9]+),\1,\1,4
**	verllg	%v24,\2,32
**	br	%r14
*/

v4si
rev_v4si_rm (v4si *x)
{
  v4si m = { 3, 2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v4si_rm_z15:
**	vlerf	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v4si
rev_v4si_rm_z15 (v4si *x)
{
  v4si m = { 3, 2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v4si_mr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	verllg	(%v[0-9]+),\1,32
**	vst	\2,0\(%r2\),3
**	br	%r14
*/

void
rev_v4si_mr (v4si *x, v4si y)
{
  v4si m = { 3, 2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v4si_mr_z15:
**	vsterf	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v4si_mr_z15 (v4si *x, v4si y)
{
  v4si m = { 3, 2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v2di_rr:
**	vpdi	%v24,%v24,%v24,4
**	br	%r14
*/

v2di
rev_v2di_rr (v2di x)
{
  v2di m = { 1, 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v2di_rm:
**	vl	(%v[0-9]+),0\(%r2\),3
**	vpdi	%v24,\1,\1,4
**	br	%r14
*/

v2di
rev_v2di_rm (v2di *x)
{
  v2di m = { 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v2di_rm_z15:
**	vlerg	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v2di
rev_v2di_rm_z15 (v2di *x)
{
  v2di m = { 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v2di_mr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	vst	\1,0\(%r2\),3
**	br	%r14
*/

void
rev_v2di_mr (v2di *x, v2di y)
{
  v2di m = { 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v2di_mr_z15:
**	vsterg	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v2di_mr_z15 (v2di *x, v2di y)
{
  v2di m = { 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v1ti_rr:
**	br	%r14
*/

v1ti
rev_v1ti_rr (v1ti x)
{
  v1ti m = { 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v1ti_rm:
**	vl	%v24,0\(%r2\),3
**	br	%r14
*/

v1ti
rev_v1ti_rm (v1ti *x)
{
  v1ti m = { 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v1ti_rm_z15:
**	vl	%v24,0\(%r2\),3
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v1ti
rev_v1ti_rm_z15 (v1ti *x)
{
  v1ti m = { 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v1ti_mr:
**	vst	%v24,0\(%r2\),3
**	br	%r14
*/

void
rev_v1ti_mr (v1ti *x, v1ti y)
{
  v1ti m = { 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v1ti_mr_z15:
**	vst	%v24,0\(%r2\),3
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v1ti_mr_z15 (v1ti *x, v1ti y)
{
  v1ti m = { 0 };
  *x = __builtin_shuffle (y, m);
}

/* FLOAT */

/*
** rev_v8hf_rr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	verllg	(%v[0-9]+),\1,32
**	verllf	%v24,\2,16
**	br	%r14
*/

v8hf
rev_v8hf_rr (v8hf x)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v8hf_rm:
**	vl	(%v[0-9]+),0\(%r2\),3
**	vpdi	(%v[0-9]+),\1,\1,4
**	verllg	(%v[0-9]+),\2,32
**	verllf	%v24,\3,16
**	br	%r14
*/

v8hf
rev_v8hf_rm (v8hf *x)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v8hf_rm_z15:
**	vlerh	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v8hf
rev_v8hf_rm_z15 (v8hf *x)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v8hf_mr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	verllg	(%v[0-9]+),\1,32
**	verllf	(%v[0-9]+),\2,16
**	vst	\3,0\(%r2\),3
**	br	%r14
*/

void
rev_v8hf_mr (v8hf *x, v8hf y)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v8hf_mr_z15:
**	vsterh	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v8hf_mr_z15 (v8hf *x, v8hf y)
{
  v8hi m = { 7, 6, 5, 4, 3, 2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v4sf_rr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	verllg	%v24,\1,32
**	br	%r14
*/

v4sf
rev_v4sf_rr (v4sf x)
{
  v4si m = { 3, 2, 1, 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v4sf_rm:
**	vl	(%v[0-9]+),0\(%r2\),3
**	vpdi	(%v[0-9]+),\1,\1,4
**	verllg	%v24,\2,32
**	br	%r14
*/

v4sf
rev_v4sf_rm (v4sf *x)
{
  v4si m = { 3, 2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v4sf_rm_z15:
**	vlerf	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v4sf
rev_v4sf_rm_z15 (v4sf *x)
{
  v4si m = { 3, 2, 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v4sf_mr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	verllg	(%v[0-9]+),\1,32
**	vst	\2,0\(%r2\),3
**	br	%r14
*/

void
rev_v4sf_mr (v4sf *x, v4sf y)
{
  v4si m = { 3, 2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v4sf_mr_z15:
**	vsterf	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v4sf_mr_z15 (v4sf *x, v4sf y)
{
  v4si m = { 3, 2, 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v2df_rr:
**	vpdi	%v24,%v24,%v24,4
**	br	%r14
*/

v2df
rev_v2df_rr (v2df x)
{
  v2di m = { 1, 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v2df_rm:
**	vl	(%v[0-9]+),0\(%r2\),3
**	vpdi	%v24,\1,\1,4
**	br	%r14
*/

v2df
rev_v2df_rm (v2df *x)
{
  v2di m = { 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v2df_rm_z15:
**	vlerg	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v2df
rev_v2df_rm_z15 (v2df *x)
{
  v2di m = { 1, 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v2df_mr:
**	vpdi	(%v[0-9]+),%v24,%v24,4
**	vst	\1,0\(%r2\),3
**	br	%r14
*/

void
rev_v2df_mr (v2df *x, v2df y)
{
  v2di m = { 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v2df_mr_z15:
**	vsterg	%v24,0\(%r2\)
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v2df_mr_z15 (v2df *x, v2df y)
{
  v2di m = { 1, 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v1tf_rr:
**	br	%r14
*/

v1tf
rev_v1tf_rr (v1tf x)
{
  v1ti m = { 0 };
  return __builtin_shuffle (x, m);
}

/*
** rev_v1tf_rm:
**	vl	%v24,0\(%r2\),3
**	br	%r14
*/

v1tf
rev_v1tf_rm (v1tf *x)
{
  v1ti m = { 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v1tf_rm_z15:
**	vl	%v24,0\(%r2\),3
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
v1tf
rev_v1tf_rm_z15 (v1tf *x)
{
  v1ti m = { 0 };
  return __builtin_shuffle (*x, m);
}

/*
** rev_v1tf_mr:
**	vst	%v24,0\(%r2\),3
**	br	%r14
*/

void
rev_v1tf_mr (v1tf *x, v1tf y)
{
  v1ti m = { 0 };
  *x = __builtin_shuffle (y, m);
}

/*
** rev_v1tf_mr_z15:
**	vst	%v24,0\(%r2\),3
**	br	%r14
*/

__attribute__ ((target ("arch=z15")))
void
rev_v1tf_mr_z15 (v1tf *x, v1tf y)
{
  v1ti m = { 0 };
  *x = __builtin_shuffle (y, m);
}
