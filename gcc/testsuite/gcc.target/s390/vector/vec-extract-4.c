/* { dg-do compile { target float16 } } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target s390_mvx } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef _Float16 __attribute__ ((vector_size  (2))) v1hf;
typedef _Float16 __attribute__ ((vector_size  (4))) v2hf;
typedef _Float16 __attribute__ ((vector_size  (8))) v4hf;
typedef _Float16 __attribute__ ((vector_size (16))) v8hf;



/************
 * REGISTER *
 ************/



/*
** vec_extract_first_v1hf:
**	vlr	%v0,%v24
**	br	%r14
*/

/*
** vec_extract_first_v2hf:
**	vlr	%v0,%v24
**	br	%r14
*/

/*
** vec_extract_first_v4hf:
**	vlr	%v0,%v24
**	br	%r14
*/

/*
** vec_extract_first_v8hf:
**	vlr	%v0,%v24
**	br	%r14
*/

_Float16 vec_extract_first_v1hf (v1hf x) { return x[0]; }
_Float16 vec_extract_first_v2hf (v2hf x) { return x[0]; }
_Float16 vec_extract_first_v4hf (v4hf x) { return x[0]; }
_Float16 vec_extract_first_v8hf (v8hf x) { return x[0]; }

/*
** vec_extract_second_v2hf:
**	vreph	%v0,%v24,1
**	br	%r14
*/

/*
** vec_extract_second_v4hf:
**	vreph	%v0,%v24,1
**	br	%r14
*/

/*
** vec_extract_second_v8hf:
**	vreph	%v0,%v24,1
**	br	%r14
*/

_Float16 vec_extract_second_v2hf (v2hf x) { return x[1]; }
_Float16 vec_extract_second_v4hf (v4hf x) { return x[1]; }
_Float16 vec_extract_second_v8hf (v8hf x) { return x[1]; }

/*
** vec_extract_third_v4hf:
**	vreph	%v0,%v24,2
**	br	%r14
*/

/*
** vec_extract_third_v8hf:
**	vreph	%v0,%v24,2
**	br	%r14
*/

_Float16 vec_extract_third_v4hf (v4hf x) { return x[2]; }
_Float16 vec_extract_third_v8hf (v8hf x) { return x[2]; }

/*
** vec_extract_fourth_v4hf:
**	vreph	%v0,%v24,3
**	br	%r14
*/

/*
** vec_extract_fourth_v8hf:
**	vreph	%v0,%v24,3
**	br	%r14
*/

_Float16 vec_extract_fourth_v4hf (v4hf x) { return x[3]; }
_Float16 vec_extract_fourth_v8hf (v8hf x) { return x[3]; }

/*
** vec_extract_fifth_v8hf:
**	vreph	%v0,%v24,4
**	br	%r14
*/

_Float16 vec_extract_fifth_v8hf (v8hf x) { return x[4]; }

/*
** vec_extract_sixth_v8hf:
**	vreph	%v0,%v24,5
**	br	%r14
*/

_Float16 vec_extract_sixth_v8hf (v8hf x) { return x[5]; }

/*
** vec_extract_seventh_v8hf:
**	vreph	%v0,%v24,6
**	br	%r14
*/

_Float16 vec_extract_seventh_v8hf (v8hf x) { return x[6]; }

/*
** vec_extract_eighth_v8hf:
**	vreph	%v0,%v24,7
**	br	%r14
*/

_Float16 vec_extract_eighth_v8hf (v8hf x) { return x[7]; }

/*
** vec_extract_nth_plus_v1hf:
**	vlgvh	(%r[0-9]+),%v24,3\(%r2\)
**	vlvgh	%v0,\1,0
**	br	%r14
*/

_Float16 vec_extract_nth_plus_v1hf (v8hf x, int n) { return x[n + 3]; }

/*
** vec_extract_nth_plus_v2hf:
**	vlgvh	(%r[0-9]+),%v24,3\(%r2\)
**	vlvgh	%v0,\1,0
**	br	%r14
*/

_Float16 vec_extract_nth_plus_v2hf (v8hf x, int n) { return x[n + 3]; }

/*
** vec_extract_nth_plus_v4hf:
**	vlgvh	(%r[0-9]+),%v24,3\(%r2\)
**	vlvgh	%v0,\1,0
**	br	%r14
*/

_Float16 vec_extract_nth_plus_v4hf (v8hf x, int n) { return x[n + 3]; }

/*
** vec_extract_nth_plus_v8hf:
**	vlgvh	(%r[0-9]+),%v24,3\(%r2\)
**	vlvgh	%v0,\1,0
**	br	%r14
*/

_Float16 vec_extract_nth_plus_v8hf (v8hf x, int n) { return x[n + 3]; }



/**********
 * MEMORY *
 **********/



/*
** vec_extract_first_v1hf_mem:
**	vsteh	%v24,0\(%r2\),0
**	br	%r14
*/

/*
** vec_extract_first_v2hf_mem:
**	vsteh	%v24,0\(%r2\),0
**	br	%r14
*/

/*
** vec_extract_first_v4hf_mem:
**	vsteh	%v24,0\(%r2\),0
**	br	%r14
*/

/*
** vec_extract_first_v8hf_mem:
**	vsteh	%v24,0\(%r2\),0
**	br	%r14
*/

void vec_extract_first_v1hf_mem (_Float16 *r, v1hf x) { *r = x[0]; }
void vec_extract_first_v2hf_mem (_Float16 *r, v2hf x) { *r = x[0]; }
void vec_extract_first_v4hf_mem (_Float16 *r, v4hf x) { *r = x[0]; }
void vec_extract_first_v8hf_mem (_Float16 *r, v8hf x) { *r = x[0]; }

/*
** vec_extract_second_v2hf_mem:
**	vsteh	%v24,0\(%r2\),1
**	br	%r14
*/

/*
** vec_extract_second_v4hf_mem:
**	vsteh	%v24,0\(%r2\),1
**	br	%r14
*/

/*
** vec_extract_second_v8hf_mem:
**	vsteh	%v24,0\(%r2\),1
**	br	%r14
*/

void vec_extract_second_v2hf_mem (_Float16 *r, v2hf x) { *r = x[1]; }
void vec_extract_second_v4hf_mem (_Float16 *r, v4hf x) { *r = x[1]; }
void vec_extract_second_v8hf_mem (_Float16 *r, v8hf x) { *r = x[1]; }

/*
** vec_extract_third_v4hf_mem:
**	vsteh	%v24,0\(%r2\),2
**	br	%r14
*/

/*
** vec_extract_third_v8hf_mem:
**	vsteh	%v24,0\(%r2\),2
**	br	%r14
*/

void vec_extract_third_v4hf_mem (_Float16 *r, v4hf x) { *r = x[2]; }
void vec_extract_third_v8hf_mem (_Float16 *r, v8hf x) { *r = x[2]; }

/*
** vec_extract_fourth_v4hf_mem:
**	vsteh	%v24,0\(%r2\),3
**	br	%r14
*/

/*
** vec_extract_fourth_v8hf_mem:
**	vsteh	%v24,0\(%r2\),3
**	br	%r14
*/

void vec_extract_fourth_v4hf_mem (_Float16 *r, v4hf x) { *r = x[3]; }
void vec_extract_fourth_v8hf_mem (_Float16 *r, v8hf x) { *r = x[3]; }

/*
** vec_extract_fifth_v8hf_mem:
**	vsteh	%v24,0\(%r2\),4
**	br	%r14
*/

void vec_extract_fifth_v8hf_mem (_Float16 *r, v8hf x) { *r = x[4]; }

/*
** vec_extract_sixth_v8hf_mem:
**	vsteh	%v24,0\(%r2\),5
**	br	%r14
*/

void vec_extract_sixth_v8hf_mem (_Float16 *r, v8hf x) { *r = x[5]; }

/*
** vec_extract_seventh_v8hf_mem:
**	vsteh	%v24,0\(%r2\),6
**	br	%r14
*/

void vec_extract_seventh_v8hf_mem (_Float16 *r, v8hf x) { *r = x[6]; }

/*
** vec_extract_eighth_v8hf_mem:
**	vsteh	%v24,0\(%r2\),7
**	br	%r14
*/

void vec_extract_eighth_v8hf_mem (_Float16 *r, v8hf x) { *r = x[7]; }

/*
** vec_extract_nth_plus_v1hf_mem:
**	vlgvh	(%r[0-9]+),%v24,3\(%r3\)
**	sth	\1,0\(%r2\)
**	br	%r14
*/

/*
** vec_extract_nth_plus_v2hf_mem:
**	vlgvh	(%r[0-9]+),%v24,3\(%r3\)
**	sth	\1,0\(%r2\)
**	br	%r14
*/

/*
** vec_extract_nth_plus_v4hf_mem:
**	vlgvh	(%r[0-9]+),%v24,3\(%r3\)
**	sth	\1,0\(%r2\)
**	br	%r14
*/

/*
** vec_extract_nth_plus_v8hf_mem:
**	vlgvh	(%r[0-9]+),%v24,3\(%r3\)
**	sth	\1,0\(%r2\)
**	br	%r14
*/

void vec_extract_nth_plus_v1hf_mem (_Float16 *r, v1hf x, int n) { *r = x[n + 3]; }
void vec_extract_nth_plus_v2hf_mem (_Float16 *r, v2hf x, int n) { *r = x[n + 3]; }
void vec_extract_nth_plus_v4hf_mem (_Float16 *r, v4hf x, int n) { *r = x[n + 3]; }
void vec_extract_nth_plus_v8hf_mem (_Float16 *r, v8hf x, int n) { *r = x[n + 3]; }
