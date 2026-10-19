/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-final { scan-assembler-times {\n\tvzero\t} 3 } } */

typedef unsigned char __attribute__ ((vector_size (16))) v16qi;
typedef unsigned short __attribute__ ((vector_size (16))) v8hi;
typedef unsigned int __attribute__ ((vector_size (16))) v4si;
typedef unsigned long __attribute__ ((vector_size (16))) v2di;
typedef unsigned __int128 __attribute__ ((vector_size (16))) v1ti;

/*
** clzsi_z900:
** ...
**	brasl	%r14,__clzdi2@PLT
** ...
*/

__attribute__ ((target ("arch=z900")))
int clzsi_z900 (unsigned int x)
{
  return x ? __builtin_clz (x) : 32;
}

/*
** clzdi_z900:
** ...
**	brasl	%r14,__clzdi2@PLT
** ...
*/

__attribute__ ((target ("arch=z900")))
int clzdi_z900 (unsigned long x)
{
  return x ? __builtin_clzl (x) : 64;
}

/*
** clzsi_z9_109:
**	flogr	%r2,%r2
**	ahi	%r2,-32
**	lgfr	%r2,%r2
**	br	%r14
*/

__attribute__ ((target ("arch=z9-109")))
int clzsi_z9_109 (unsigned int x)
{
  return x ? __builtin_clz (x) : 32;
}

/*
** clzdi_z9_109:
**	flogr	%r2,%r2
**	lgfr	%r2,%r2
**	br	%r14
*/

__attribute__ ((target ("arch=z9-109")))
int clzdi_z9_109 (unsigned long x)
{
  return x ? __builtin_clzl (x) : 64;
}

/*
** clzsi_z17:
**	clzg	%r2,%r2
**	ahi	%r2,-32
**	lgfr	%r2,%r2
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
int clzsi_z17 (unsigned int x)
{
  return x ? __builtin_clz (x) : 32;
}

/*
** clzdi_z17:
**	clzg	%r2,%r2
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
int clzdi_z17 (unsigned long x)
{
  return x ? __builtin_clzl (x) : 64;
}

/*
** clzti_z17:
**	vl	(%v[0-9]+),0\(%r2\),3
**	vclzq	(%v[0-9]+),\1
**	vlgvf	(%r[0-9]+),\2,3
**	lgfr	%r2,\3
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
int clzti_z17 (unsigned __int128 x)
{
  return x ? __builtin_clzg (x) : 128;
}

/*
** clzsi_z13_autovec:
**	...
**	vclzf	.*
**	...
**	vst	.*
**	...
*/

__attribute__ ((target ("arch=z13")))
void clzsi_z13_autovec (unsigned int *x)
{
  for (int i = 0; i < 64; ++i)
    x[i] = x[i] ? __builtin_clz (x[i]) : 32;
}

/*
** clzdi_z13_autovec:
**	...
**	vclzg	.*
**	...
**	vpkg	.*
**	...
**	vst	.*
**	...
*/

__attribute__ ((target ("arch=z13")))
void clzdi_z13_autovec (int *x, unsigned long *y)
{
  for (int i = 0; i < 64; ++i)
    x[i] = y[i] ? __builtin_clzl (y[i]) : 64;
}

/*
** clzti_z17_autovec:
**	...
**	vclzq	.*
**	...
**	vstef	.*
**	...
*/

__attribute__ ((target ("arch=z17")))
void clzti_z17_autovec (int *x, unsigned __int128 *y)
{
  for (int i = 0; i < 64; ++i)
    x[i] = y[i] ? __builtin_clzg (y[i]) : 128;
}

/* For __builtin_clz arguments are promoted to unsigned int which is why we are
   faced with an extra addend .CLZ(x, 8) + 24.

** clzv16qi_z13:
** ...
**	vzero	.*
** ...
**	vclzb	%v[0-9]+,%v[0-9]+
** ...
*/

__attribute__ ((target ("arch=z13")))
v16qi clzv16qi_z13 (v16qi x)
{
  for (int i = 0; i < 16; ++i)
    x[i] = x[i] ? __builtin_clz (x[i]) : 8;
  return x;
}

/*
** clzgv16qi_z13:
**	vclzb	%v24,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z13")))
v16qi clzgv16qi_z13 (v16qi x)
{
  for (int i = 0; i < 16; ++i)
    x[i] = x[i] ? __builtin_clzg (x[i]) : 8;
  return x;
}

/* For __builtin_clz arguments are promoted to unsigned int which is why we are
   faced with an extra addend .CLZ(x, 16) + 16.

** clzv8hi_z13:
** ...
**	vzero	.*
** ...
**	vclzh	%v[0-9]+,%v[0-9]+
** ...
*/

__attribute__ ((target ("arch=z13")))
v8hi clzv8hi_z13 (v8hi x)
{
  for (int i = 0; i < 8; ++i)
    x[i] = x[i] ? __builtin_clz (x[i]) : 16;
  return x;
}

/*
** clzgv8hi_z13:
**	vclzh	%v24,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z13")))
v8hi clzgv8hi_z13 (v8hi x)
{
  for (int i = 0; i < 8; ++i)
    x[i] = x[i] ? __builtin_clzg (x[i]) : 16;
  return x;
}

/*
** clzv4si_z13:
**	vclzf	%v24,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z13")))
v4si clzv4si_z13 (v4si x)
{
  for (int i = 0; i < 4; ++i)
    x[i] = x[i] ? __builtin_clz (x[i]) : 32;
  return x;
}



/*
** ctzsi_z900:
** ...
**	brasl	%r14,__ctzdi2@PLT
** ...
*/

__attribute__ ((target ("arch=z900")))
int ctzsi_z900 (unsigned int x)
{
  return x ? __builtin_ctz (x) : 32;
}

/*
** ctzdi_z900:
** ...
**	brasl	%r14,__ctzdi2@PLT
** ...
*/

__attribute__ ((target ("arch=z900")))
int ctzdi_z900 (unsigned long x)
{
  return x ? __builtin_ctzl (x) : 64;
}

/*
** ctzsi_z9_109:
** ...
**	flogr	%r[0-9]+,%r[0-9]+
** ...
*/

__attribute__ ((target ("arch=z9-109")))
int ctzsi_z9_109 (unsigned int x)
{
  return x ? __builtin_ctz (x) : 32;
}

/*
** ctzdi_z9_109:
** ...
**	flogr	%r[0-9]+,%r[0-9]+
** ...
*/

__attribute__ ((target ("arch=z9-109")))
int ctzdi_z9_109 (unsigned long x)
{
  return x ? __builtin_ctzl (x) : 64;
}

/*
** ctzsi_z17:
**	oihl	%r2,1
**	ctzg	%r2,%r2
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
int ctzsi_z17 (unsigned int x)
{
  return x ? __builtin_ctz (x) : 32;
}

/*
** ctzdi_z17:
**	ctzg	%r2,%r2
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
int ctzdi_z17 (unsigned long x)
{
  return x ? __builtin_ctzl (x) : 64;
}

/*
** ctzti_z17:
**	vl	(%v[0-9]+),0\(%r2\),3
**	vctzq	(%v[0-9]+),\1
**	vlgvf	(%r[0-9]+),\2,3
**	lgfr	%r2,\3
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
int ctzti_z17 (unsigned __int128 x)
{
  return x ? __builtin_ctzg (x) : 128;
}

/*
** ctzsi_z13_autovec:
**	...
**	vctzf	.*
**	...
**	vst	.*
**	...
*/

__attribute__ ((target ("arch=z13")))
void ctzsi_z13_autovec (unsigned int *x)
{
  for (int i = 0; i < 64; ++i)
    x[i] = x[i] ? __builtin_ctz (x[i]) : 32;
}

/* TODO: fold zero test.
   In contrast to ctzsi_z13_autovec, vect-pattern recognition fails because
   type precision differs for lhs and promoted rhs.  Consequently no ctz ifn is
   constructed which is required by match.pd in order to fold a zero test.
   Furthermore, in contrast to ctzdi_z17_autovec where phiopt2 already folds
   the zero test since starting with z17 ctzdi2 becomes available and
   vect-patterns recog never sees a zero test.

** ctzdi_z13_autovec:
**	...
**	vzero	.*
**	...
**	vctzg	.*
**	...
**	vpkg	.*
**	...
**	vst	.*
**	...
*/

__attribute__ ((target ("arch=z13")))
void ctzdi_z13_autovec (int *x, unsigned long *y)
{
  for (int i = 0; i < 64; ++i)
    x[i] = y[i] ? __builtin_ctzl (y[i]) : 64;
}

/*
** ctzdi_z17_autovec:
**	...
**	vctzg	.*
**	...
**	vpkg	.*
**	...
**	vst	.*
**	...
*/

__attribute__ ((target ("arch=z17")))
void ctzdi_z17_autovec (int *x, unsigned long *y)
{
  for (int i = 0; i < 64; ++i)
    x[i] = y[i] ? __builtin_ctzl (y[i]) : 64;
}

/*
** ctzti_z17_autovec:
**	...
**	vctzq	.*
**	...
**	vstef	.*
**	...
*/

__attribute__ ((target ("arch=z17")))
void ctzti_z17_autovec (int *x, unsigned __int128 *y)
{
  for (int i = 0; i < 64; ++i)
    x[i] = y[i] ? __builtin_ctzg (y[i]) : 128;
}


/*
** ctzv16qi_z13:
**	vctzb	%v24,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z13")))
v16qi ctzv16qi_z13 (v16qi x)
{
  for (int i = 0; i < 16; ++i)
    x[i] = x[i] ? __builtin_ctz (x[i]) : 8;
  return x;
}

/*
** ctzv8hi_z13:
**	vctzh	%v24,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z13")))
v8hi ctzv8hi_z13 (v8hi x)
{
  for (int i = 0; i < 8; ++i)
    x[i] = x[i] ? __builtin_ctz (x[i]) : 16;
  return x;
}

/*
** ctzv4si_z13:
**	vctzf	%v24,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z13")))
v4si ctzv4si_z13 (v4si x)
{
  for (int i = 0; i < 4; ++i)
    x[i] = x[i] ? __builtin_ctz (x[i]) : 32;
  return x;
}

/*
** ctzv4si_z17:
**	vctzf	%v24,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
v4si ctzv4si_z17 (v4si x)
{
  for (int i = 0; i < 4; ++i)
    x[i] = x[i] ? __builtin_ctz (x[i]) : 32;
  return x;
}
