/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -march=z9-109" } */
/* { dg-final { check-function-bodies "**" "" "" } } */



/************************************
 * Storage-and-Immediate Operations *
 ************************************/



/*
** and_char:
**	ni	0\(%r2\),246
**	br	%r14
*/
void and_char (char *x) { *x &= -10; }

/*
** and_char_v:
**	ni	0\(%r2\),246
**	br	%r14
*/
void and_char_v (volatile char *x) { *x &= -10; }

/*
** and_short:
**	ni	1\(%r2\),246
**	br	%r14
*/
void and_short (short *x) { *x &= -10; }

/*
** and_short_v:
**	lh	(%r[0-9]+),0\(%r2\)
**	nill	\1,65526
**	sth	\1,0\(%r2\)
**	br	%r14
*/
void and_short_v (volatile short *x) { *x &= -10; }

/*
** and_int:
**	ni	3\(%r2\),246
**	br	%r14
*/
void and_int (int *x) { *x &= -10; }

/*
** and_int_v:
**	l	(%r[0-9]+),0\(%r2\)
**	nill	\1,65526
**	st	\1,0\(%r2\)
**	br	%r14
*/
void and_int_v (volatile int *x) { *x &= -10; }

/*
** and_long:
**	ni	7\(%r2\),246
**	br	%r14
*/
void and_long (long *x) { *x &= -10; }

/*
** and_long_v:
**	lg	(%r[0-9]+),0\(%r2\)
**	nill	\1,65526
**	stg	\1,0\(%r2\)
**	br	%r14
*/
void and_long_v (volatile long *x) { *x &= -10; }

/*
** ior_char:
**	oi	0\(%r2\),10
**	br	%r14
*/
void ior_char (char *x) { *x |= 10; }

/*
** ior_char_v:
**	oi	0\(%r2\),10
**	br	%r14
*/
void ior_char_v (volatile char *x) { *x |= 10; }

/*
** ior_short:
**	oi	1\(%r2\),10
**	br	%r14
*/
void ior_short (short *x) { *x |= 10; }

/*
** ior_short_v:
**	lh	(%r[0-9]+),0\(%r2\)
**	oill	\1,10
**	sth	\1,0\(%r2\)
**	br	%r14
*/
void ior_short_v (volatile short *x) { *x |= 10; }

/*
** ior_int:
**	oi	3\(%r2\),10
**	br	%r14
*/
void ior_int (int *x) { *x |= 10; }

/*
** ior_int_v:
**	l	(%r[0-9]+),0\(%r2\)
**	oill	\1,10
**	st	\1,0\(%r2\)
**	br	%r14
*/
void ior_int_v (volatile int *x) { *x |= 10; }

/*
** ior_long:
**	oi	7\(%r2\),10
**	br	%r14
*/
void ior_long (long *x) { *x |= 10; }

/*
** ior_long_v:
**	lg	(%r[0-9]+),0\(%r2\)
**	oill	\1,10
**	stg	\1,0\(%r2\)
**	br	%r14
*/
void ior_long_v (volatile long *x) { *x |= 10; }

/*
** xor_char:
**	xi	0\(%r2\),10
**	br	%r14
*/
void xor_char (char *x) { *x ^= 10; }

/*
** xor_char_v:
**	xi	0\(%r2\),10
**	br	%r14
*/
void xor_char_v (volatile char *x) { *x ^= 10; }

/*
** xor_short:
**	xi	1\(%r2\),10
**	br	%r14
*/
void xor_short (short *x) { *x ^= 10; }

/*
** xor_short_v:
**	lh	(%r[0-9]+),0\(%r2\)
**	xilf	\1,10
**	sth	\1,0\(%r2\)
**	br	%r14
*/
void xor_short_v (volatile short *x) { *x ^= 10; }

/*
** xor_int:
**	xi	3\(%r2\),10
**	br	%r14
*/
void xor_int (int *x) { *x ^= 10; }

/*
** xor_int_v:
**	l	(%r[0-9]+),0\(%r2\)
**	xilf	\1,10
**	st	\1,0\(%r2\)
**	br	%r14
*/
void xor_int_v (volatile int *x) { *x ^= 10; }

/*
** xor_long:
**	xi	7\(%r2\),10
**	br	%r14
*/
void xor_long (long *x) { *x ^= 10; }

/*
** xor_long_v:
**	lg	(%r[0-9]+),0\(%r2\)
**	xilf	\1,10
**	stg	\1,0\(%r2\)
**	br	%r14
*/
void xor_long_v (volatile long *x) { *x ^= 10; }



/**********************************
 * Storage-and-Storage Operations *
 **********************************/

/* <OP>_<TYPE>_v0 zero volatile operands
   <OP>_<TYPE>_v1 first operand is volatile
   <OP>_<TYPE>_v2 second operand is volatile */



/*
** and_char_v0:
**	nc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void and_char_v0 (char *x, char *y) { *x &= *y; }

/*
** and_char_v1:
**	nc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void and_char_v1 (volatile char *x, char *y) { *x &= *y; }

/*
** and_char_v2:
**	nc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void and_char_v2 (char *x, volatile char *y) { *x &= *y; }

/*
** and_short_v0:
**	nc	0\(2,%r2\),0\(%r3\)
**	br	%r14
*/
void and_short_v0 (short *x, short *y) { *x &= *y; }

/*
** and_short_v1:
**	lh	%r[0-9]+,0\(%r[23]\)
**	lh	%r[0-9]+,0\(%r[23]\)
**	nr	(%r[0-9]+),%r[0-9]+
**	sth	\1,0\(%r2\)
**	br	%r14
*/
void and_short_v1 (volatile short *x, short *y) { *x &= *y; }

/*
** and_short_v2:
**	...
**	nc	0\(2,%r2\),166\(%r15\)
**	...
*/
void and_short_v2 (short *x, volatile short *y) { *x &= *y; }

/*
** and_int_v0:
**	nc	0\(4,%r2\),0\(%r3\)
**	br	%r14
*/
void and_int_v0 (int *x, int *y) { *x &= *y; }

/*
** and_int_v1:
**	l	(%r[0-9]+),0\(%r[23]\)
**	n	\1,0\(%r[23]\)
**	st	\1,0\(%r[23]\)
**	br	%r14
*/
void and_int_v1 (volatile int *x, int *y) { *x &= *y; }

/*
** and_int_v2:
**	l	(%r[0-9]+),0\(%r[23]\)
**	n	\1,0\(%r[23]\)
**	st	\1,0\(%r[23]\)
**	br	%r14
*/
void and_int_v2 (int *x, volatile int *y) { *x &= *y; }

/*
** and_long_v0:
**	nc	0\(8,%r2\),0\(%r3\)
**	br	%r14
*/
void and_long_v0 (long *x, long *y) { *x &= *y; }

/*
** and_long_v1:
**	lg	(%r[0-9]+),0\(%r[23]\)
**	ng	\1,0\(%r[23]\)
**	stg	\1,0\(%r[23]\)
**	br	%r14
*/
void and_long_v1 (volatile long *x, long *y) { *x &= *y; }

/*
** and_long_v2:
**	lg	(%r[0-9]+),0\(%r[23]\)
**	ng	\1,0\(%r[23]\)
**	stg	\1,0\(%r[23]\)
**	br	%r14
*/
void and_long_v2 (long *x, volatile long *y) { *x &= *y; }

/*
** ior_char_v0:
**	oc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void ior_char_v0 (char *x, char *y) { *x |= *y; }

/*
** ior_char_v1:
**	oc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void ior_char_v1 (volatile char *x, char *y) { *x |= *y; }

/*
** ior_char_v2:
**	oc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void ior_char_v2 (char *x, volatile char *y) { *x |= *y; }

/*
** ior_short_v0:
**	oc	0\(2,%r2\),0\(%r3\)
**	br	%r14
*/
void ior_short_v0 (short *x, short *y) { *x |= *y; }

/*
** ior_short_v1:
**	lh	%r[0-9]+,0\(%r[23]\)
**	lh	%r[0-9]+,0\(%r[23]\)
**	or	(%r[0-9]+),%r[0-9]+
**	sth	\1,0\(%r2\)
**	br	%r14
*/
void ior_short_v1 (volatile short *x, short *y) { *x |= *y; }

/*
** ior_short_v2:
**	...
**	oc	0\(2,%r2\),166\(%r15\)
**	...
*/
void ior_short_v2 (short *x, volatile short *y) { *x |= *y; }

/*
** ior_int_v0:
**	oc	0\(4,%r2\),0\(%r3\)
**	br	%r14
*/
void ior_int_v0 (int *x, int *y) { *x |= *y; }

/*
** ior_int_v1:
**	l	(%r[0-9]+),0\(%r[23]\)
**	o	\1,0\(%r[23]\)
**	st	\1,0\(%r[23]\)
**	br	%r14
*/
void ior_int_v1 (volatile int *x, int *y) { *x |= *y; }

/*
** ior_int_v2:
**	l	(%r[0-9]+),0\(%r[23]\)
**	o	\1,0\(%r[23]\)
**	st	\1,0\(%r[23]\)
**	br	%r14
*/
void ior_int_v2 (int *x, volatile int *y) { *x |= *y; }

/*
** ior_long_v0:
**	oc	0\(8,%r2\),0\(%r3\)
**	br	%r14
*/
void ior_long_v0 (long *x, long *y) { *x |= *y; }

/*
** ior_long_v1:
**	lg	(%r[0-9]+),0\(%r[23]\)
**	og	\1,0\(%r[23]\)
**	stg	\1,0\(%r[23]\)
**	br	%r14
*/
void ior_long_v1 (volatile long *x, long *y) { *x |= *y; }

/*
** ior_long_v2:
**	lg	(%r[0-9]+),0\(%r[23]\)
**	og	\1,0\(%r[23]\)
**	stg	\1,0\(%r[23]\)
**	br	%r14
*/
void ior_long_v2 (long *x, volatile long *y) { *x |= *y; }

/*
** xor_char_v0:
**	xc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void xor_char_v0 (char *x, char *y) { *x ^= *y; }

/*
** xor_char_v1:
**	xc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void xor_char_v1 (volatile char *x, char *y) { *x ^= *y; }

/*
** xor_char_v2:
**	xc	0\(1,%r2\),0\(%r3\)
**	br	%r14
*/
void xor_char_v2 (char *x, volatile char *y) { *x ^= *y; }

/*
** xor_short_v0:
**	xc	0\(2,%r2\),0\(%r3\)
**	br	%r14
*/
void xor_short_v0 (short *x, short *y) { *x ^= *y; }

/*
** xor_short_v1:
**	lh	%r[0-9]+,0\(%r[23]\)
**	lh	%r[0-9]+,0\(%r[23]\)
**	xr	(%r[0-9]+),%r[0-9]+
**	sth	\1,0\(%r2\)
**	br	%r14
*/
void xor_short_v1 (volatile short *x, short *y) { *x ^= *y; }

/*
** xor_short_v2:
**	...
**	xc	0\(2,%r2\),166\(%r15\)
**	...
*/
void xor_short_v2 (short *x, volatile short *y) { *x ^= *y; }

/*
** xor_int_v0:
**	xc	0\(4,%r2\),0\(%r3\)
**	br	%r14
*/
void xor_int_v0 (int *x, int *y) { *x ^= *y; }

/*
** xor_int_v1:
**	l	(%r[0-9]+),0\(%r[23]\)
**	x	\1,0\(%r[23]\)
**	st	\1,0\(%r[23]\)
**	br	%r14
*/
void xor_int_v1 (volatile int *x, int *y) { *x ^= *y; }

/*
** xor_int_v2:
**	l	(%r[0-9]+),0\(%r[23]\)
**	x	\1,0\(%r[23]\)
**	st	\1,0\(%r[23]\)
**	br	%r14
*/
void xor_int_v2 (int *x, volatile int *y) { *x ^= *y; }

/*
** xor_long_v0:
**	xc	0\(8,%r2\),0\(%r3\)
**	br	%r14
*/
void xor_long_v0 (long *x, long *y) { *x ^= *y; }

/*
** xor_long_v1:
**	lg	(%r[0-9]+),0\(%r[23]\)
**	xg	\1,0\(%r[23]\)
**	stg	\1,0\(%r[23]\)
**	br	%r14
*/
void xor_long_v1 (volatile long *x, long *y) { *x ^= *y; }

/*
** xor_long_v2:
**	lg	(%r[0-9]+),0\(%r[23]\)
**	xg	\1,0\(%r[23]\)
**	stg	\1,0\(%r[23]\)
**	br	%r14
*/
void xor_long_v2 (long *x, volatile long *y) { *x ^= *y; }
