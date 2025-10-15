/* { dg-do compile } */
/* { dg-options "-O2 -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-skip-if "" { *-*-mingw* } } */

int no_arg_stack_use_callee
  [[gnu::preserve_none, gnu::noinline,
    gnu::noipa]] (int a0, int a1, int a2, int a3, int a4, int a5, int a6,
		  int a7, int a8, int a9, int a10, int a11, int a12, int a13,
		  int a14, int a15, int a16, int a17, int a18, int a19, int a20,
		  int a21, int a22, int a24);

/* Check the pcs argument order is correct. Should be x20-28, x0-7, x10-14, x9,
 * x15 and that the return arg is x0 */

/*
** no_arg_stack_use_caller:
** ...
**	mov	w15, 23
**	mov	w9, 22
**	mov	w14, 21
**	mov	w13, 20
**	mov	w12, 19
**	mov	w11, 18
**	mov	w10, 17
**	mov	w7, 16
**	mov	w6, 15
**	mov	w5, 14
**	mov	w4, 13
**	mov	w3, 12
**	mov	w2, 11
**	mov	w1, 10
**	mov	w0, 9
**	mov	w28, 8
**	mov	w27, 7
**	mov	w26, 6
**	mov	w25, 5
**	mov	w24, 4
**	mov	w23, 3
**	mov	w22, 2
**	mov	w21, 1
**	mov	w20, 0
**	bl	no_arg_stack_use_callee
**	add	w0, w0, 1
** ...
*/
int no_arg_stack_use_caller [[gnu::preserve_none]] ()
{
  return no_arg_stack_use_callee (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
				  14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
	 + 1;
}

int arg_stack_use_callee
  [[gnu::preserve_none, gnu::noinline,
    gnu::noipa]] (int a0, int a1, int a2, int a3, int a4, int a5, int a6,
		  int a7, int a8, int a9, int a10, int a11, int a12, int a13,
		  int a14, int a15, int a16, int a17, int a18, int a19, int a20,
		  int a21, int a22, int a23, int a24);

/*
** arg_stack_use_caller:
** ...
**	mov	w0, 24
**	mov	w15, 23
**	mov	w9, 22
**	mov	w14, 21
**	mov	w13, 20
**	mov	w12, 19
**	mov	w11, 18
**	mov	w10, 17
**	mov	w7, 16
**	mov	w6, 15
**	mov	w5, 14
**	mov	w4, 13
**	mov	w3, 12
**	mov	w2, 11
**	mov	w1, 10
**	mov	w28, 8
**	mov	w27, 7
**	mov	w26, 6
**	mov	w25, 5
**	mov	w24, 4
**	mov	w23, 3
**	mov	w22, 2
**	mov	w21, 1
**	mov	w20, 0
**	str	w0, \[sp\]
**	mov	w0, 9
**	bl	arg_stack_use_callee
**	add	w0, w0, 1
** ...
*/
int arg_stack_use_caller [[gnu::preserve_none]] ()
{
  return arg_stack_use_callee (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
			       15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
	 + 1;
}
