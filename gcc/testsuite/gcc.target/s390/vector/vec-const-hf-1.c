/* { dg-do compile } */
/* { dg-options "-O2 -march=z13" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef _Float16 v1hf __attribute__ ((vector_size (2)));
typedef _Float16 v2hf __attribute__ ((vector_size (4)));
typedef _Float16 v4hf __attribute__ ((vector_size (8)));
typedef _Float16 v8hf __attribute__ ((vector_size (16)));

/* A single halfword is always loaded via vrepi and cannot end up in the
   constant pool.  */

/*
** pool2:
**	larl	(%r[0-9]+),.L[0-9]+
**	vlef	%v24,.L[0-9]+-.L[0-9]+\(\1\),0
**	br	%r14
*/
v2hf pool2 () { return (v2hf){ 1, 2 }; }

/*
** pool4:
**	larl	(%r[0-9]+),.L[0-9]+
**	vleg	%v24,.L[0-9]+-.L[0-9]+\(\1\),0
**	br	%r14
*/
v4hf pool4 () { return (v4hf){ 1, 2, 3, 4 }; }

/*
** pool8:
**	larl	(%r[0-9]+),.L[0-9]+
**	vl	%v24,.L[0-9]+-.L[0-9]+\(\1\),3
**	br	%r14
*/
v8hf pool8 () { return (v8hf){ 1, 2, 3, 4, 5, 6, 7, 8 }; }

/* vrepi: 0x1.8d0p-11 is 0x1234 which equals 4660.  */

/*
** repi1:
**	vrepi	%v24,4660,1
**	br	%r14
*/
v1hf repi1 () { return (v1hf){ 0x1.8d0p-11f16 }; }

/*
** repi2:
**	vrepi	%v24,4660,1
**	br	%r14
*/
v2hf repi2 () { return (v2hf){ 0x1.8d0p-11f16, 0x1.8d0p-11f16 }; }

/*
** repi4:
**	vrepi	%v24,4660,1
**	br	%r14
*/
v4hf repi4 ()
{
  return (v4hf){ 0x1.8d0p-11f16, 0x1.8d0p-11f16,
		 0x1.8d0p-11f16, 0x1.8d0p-11f16 };
}

/*
** repi8:
**	vrepi	%v24,4660,1
**	br	%r14
*/
v8hf repi8 ()
{
  return (v8hf){ 0x1.8d0p-11f16, 0x1.8d0p-11f16,
		 0x1.8d0p-11f16, 0x1.8d0p-11f16,
		 0x1.8d0p-11f16, 0x1.8d0p-11f16,
		 0x1.8d0p-11f16, 0x1.8d0p-11f16 };
}

/* vgm: 1.0 is 0x3c00.  */

/*
** gm1:
**	vgm	%v24,2,5,1
**	br	%r14
*/
v1hf gm1 () { return (v1hf){ 1 }; }

/*
** gm2:
**	vgm	%v24,2,5,1
**	br	%r14
*/
v2hf gm2 () { return (v2hf){ 1, 1 }; }

/*
** gm4:
**	vgm	%v24,2,5,1
**	br	%r14
*/
v4hf gm4 () { return (v4hf){ 1, 1, 1, 1 }; }

/*
** gm8:
**	vgm	%v24,2,5,1
**	br	%r14
*/
v8hf gm8 () { return (v8hf){ 1, 1, 1, 1, 1, 1, 1, 1 }; }

/* vgbm: 0x1.fep-17 is 0x00ff.  */

/*
** gbm1:
**	vgbm	%v24,16384
**	br	%r14
*/
v1hf gbm1 () { return (v1hf){ 0x1.fep-17f16 }; }

/*
** gbm2:
**	vgbm	%v24,16384
**	br	%r14
*/
v2hf gbm2 () { return (v2hf){ 0x1.fep-17f16, 0 }; }

/*
** gbm4:
**	vgbm	%v24,17408
**	br	%r14
*/
v4hf gbm4 ()
{
  return (v4hf){ 0x1.fep-17f16, 0, 0x1.fep-17f16, 0 };
}

/*
** gbm8:
**	vgbm	%v24,17476
**	br	%r14
*/
v8hf gbm8 ()
{
  return (v8hf){ 0x1.fep-17f16, 0, 0x1.fep-17f16, 0,
		 0x1.fep-17f16, 0, 0x1.fep-17f16, 0 };
}
