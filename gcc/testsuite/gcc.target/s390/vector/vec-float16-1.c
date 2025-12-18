/* { dg-do compile { target float16 } } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target s390_mvx } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* ABI tests.  Arguments and return value are passed via vector registers for
   vector sizes less than or equal to 16 bytes.  Larger vectors or more than 8
   vectors are passed via reference.  */

typedef _Float16 __attribute__ ((vector_size  (2))) v1hf;
typedef _Float16 __attribute__ ((vector_size  (4))) v2hf;
typedef _Float16 __attribute__ ((vector_size  (8))) v4hf;
typedef _Float16 __attribute__ ((vector_size (16))) v8hf;
typedef _Float16 __attribute__ ((vector_size (32))) v16hf;

#define ZERO_v1hf (v1hf){0}
#define ZERO_v2hf (v2hf){0,0}
#define ZERO_v4hf (v4hf){0,0,0,0}
#define ZERO_v8hf (v8hf){0,0,0,0,0,0,0,0}

#define T(V) \
  V V##_callee_arg_1 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x1; } \
  V V##_callee_arg_2 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x2; } \
  V V##_callee_arg_3 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x3; } \
  V V##_callee_arg_4 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x4; } \
  V V##_callee_arg_5 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x5; } \
  V V##_callee_arg_6 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x6; } \
  V V##_callee_arg_7 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x7; } \
  V V##_callee_arg_8 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x8; } \
  V V##_callee_arg_9 (V x1, V x2, V x3, V x4, V x5, V x6, V x7, V x8, V x9) { return x9; } \
  void V##_caller_nargs_9 (void) \
  { \
    V x = ZERO_##V; \
    extern void V##_fun_9 (V, V, V, V, V, V, V, V, V); \
    V##_fun_9 (x, x, x, x, x, x, x, x, x); \
  }



/********
 * V1HF *
 ********/

T (v1hf)

/*
** v1hf_callee_arg_1:
**	br	%r14
*/

/*
** v1hf_callee_arg_2:
**	vlr	%v24,%v26
**	br	%r14
*/

/*
** v1hf_callee_arg_3:
**	vlr	%v24,%v28
**	br	%r14
*/

/*
** v1hf_callee_arg_4:
**	vlr	%v24,%v30
**	br	%r14
*/

/*
** v1hf_callee_arg_5:
**	vlr	%v24,%v25
**	br	%r14
*/

/*
** v1hf_callee_arg_6:
**	vlr	%v24,%v27
**	br	%r14
*/

/*
** v1hf_callee_arg_7:
**	vlr	%v24,%v29
**	br	%r14
*/

/*
** v1hf_callee_arg_8:
**	vlr	%v24,%v31
**	br	%r14
*/

/*
** v1hf_callee_arg_9:
**	vleh	%v24,160\(%r15\),0
**	br	%r14
*/

/*
** v1hf_caller_nargs_9:
**	stmg	%r14,%r15,112\(%r15\)
**	vzero	%v31
**	lay	%r15,-168\(%r15\)
**	mvhhi	160\(%r15\),0
**	vlr	%v29,%v31
**	vlr	%v27,%v31
**	vlr	%v25,%v31
**	vlr	%v30,%v31
**	vlr	%v28,%v31
**	vlr	%v26,%v31
**	vlr	%v24,%v31
**	brasl	%r14,v1hf_fun_9@PLT
**	lmg	%r14,%r15,280\(%r15\)
**	br	%r14
*/



/********
 * V2HF *
 ********/

T (v2hf)

/*
** v2hf_callee_arg_1:
**	br	%r14
*/

/*
** v2hf_callee_arg_2:
**	vlr	%v24,%v26
**	br	%r14
*/

/*
** v2hf_callee_arg_3:
**	vlr	%v24,%v28
**	br	%r14
*/

/*
** v2hf_callee_arg_4:
**	vlr	%v24,%v30
**	br	%r14
*/

/*
** v2hf_callee_arg_5:
**	vlr	%v24,%v25
**	br	%r14
*/

/*
** v2hf_callee_arg_6:
**	vlr	%v24,%v27
**	br	%r14
*/

/*
** v2hf_callee_arg_7:
**	vlr	%v24,%v29
**	br	%r14
*/

/*
** v2hf_callee_arg_8:
**	vlr	%v24,%v31
**	br	%r14
*/

/*
** v2hf_callee_arg_9:
**	vlef	%v24,160\(%r15\),0
**	br	%r14
*/

/*
** v2hf_caller_nargs_9:
**	stmg	%r14,%r15,112\(%r15\)
**	vzero	%v31
**	lay	%r15,-168\(%r15\)
**	mvhi	160\(%r15\),0
**	vlr	%v29,%v31
**	vlr	%v27,%v31
**	vlr	%v25,%v31
**	vlr	%v30,%v31
**	vlr	%v28,%v31
**	vlr	%v26,%v31
**	vlr	%v24,%v31
**	brasl	%r14,v2hf_fun_9@PLT
**	lmg	%r14,%r15,280\(%r15\)
**	br	%r14
*/



/********
 * V4HF *
 ********/

T (v4hf)

/*
** v4hf_callee_arg_1:
**	br	%r14
*/

/*
** v4hf_callee_arg_2:
**	vlr	%v24,%v26
**	br	%r14
*/

/*
** v4hf_callee_arg_3:
**	vlr	%v24,%v28
**	br	%r14
*/

/*
** v4hf_callee_arg_4:
**	vlr	%v24,%v30
**	br	%r14
*/

/*
** v4hf_callee_arg_5:
**	vlr	%v24,%v25
**	br	%r14
*/

/*
** v4hf_callee_arg_6:
**	vlr	%v24,%v27
**	br	%r14
*/

/*
** v4hf_callee_arg_7:
**	vlr	%v24,%v29
**	br	%r14
*/

/*
** v4hf_callee_arg_8:
**	vlr	%v24,%v31
**	br	%r14
*/

/*
** v4hf_callee_arg_9:
**	vleg	%v24,160\(%r15\),0
**	br	%r14
*/

/*
** v4hf_caller_nargs_9:
**	stmg	%r14,%r15,112\(%r15\)
**	vzero	%v31
**	lay	%r15,-168\(%r15\)
**	mvghi	160\(%r15\),0
**	vlr	%v29,%v31
**	vlr	%v27,%v31
**	vlr	%v25,%v31
**	vlr	%v30,%v31
**	vlr	%v28,%v31
**	vlr	%v26,%v31
**	vlr	%v24,%v31
**	brasl	%r14,v4hf_fun_9@PLT
**	lmg	%r14,%r15,280\(%r15\)
**	br	%r14
*/



/********
 * V8HF *
 ********/

T (v8hf)

/*
** v8hf_callee_arg_1:
**	br	%r14
*/

/*
** v8hf_callee_arg_2:
**	vlr	%v24,%v26
**	br	%r14
*/

/*
** v8hf_callee_arg_3:
**	vlr	%v24,%v28
**	br	%r14
*/

/*
** v8hf_callee_arg_4:
**	vlr	%v24,%v30
**	br	%r14
*/

/*
** v8hf_callee_arg_5:
**	vlr	%v24,%v25
**	br	%r14
*/

/*
** v8hf_callee_arg_6:
**	vlr	%v24,%v27
**	br	%r14
*/

/*
** v8hf_callee_arg_7:
**	vlr	%v24,%v29
**	br	%r14
*/

/*
** v8hf_callee_arg_8:
**	vlr	%v24,%v31
**	br	%r14
*/

/*
** v8hf_callee_arg_9:
**	vl	%v24,160\(%r15\),3
**	br	%r14
*/

/*
** v8hf_caller_nargs_9:
**	stmg	%r14,%r15,112\(%r15\)
**	vzero	%v31
**	lay	%r15,-176\(%r15\)
**	vst	%v31,160\(%r15\),3
**	vlr	%v29,%v31
**	vlr	%v27,%v31
**	vlr	%v25,%v31
**	vlr	%v30,%v31
**	vlr	%v28,%v31
**	vlr	%v26,%v31
**	vlr	%v24,%v31
**	brasl	%r14,v8hf_fun_9@PLT
**	lmg	%r14,%r15,288\(%r15\)
**	br	%r14
*/



/*********
 * V16HF *
 *********/

/*
** v16hf_callee_arg_2:
**	mvc	0\(16,%r2\),0\(%r4\)
**	mvc	16\(16,%r2\),16\(%r4\)
**	br	%r14
*/

v16hf
v16hf_callee_arg_2 (v16hf x1, v16hf x2)
{
  return x2;
}
