/* { dg-do compile } */
/* { dg-options "-std=gnu99 " } */
/* { dg-additional-options "-O -fno-schedule-insns -fno-schedule-insns2 " } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-skip-if "" { *-*-mingw* } } */

typedef struct es {} Empty;

__attribute__ ((__noinline__)) void
aarchpcs_overflow (int, int, int, int, int, int, int, int, Empty);

/*
**aarchpcs_overflow_call:
**	...
**	mov	w7, 7
**	mov	w6, 6
**	mov	w5, 5
**	mov	w4, 4
**	mov	w3, 3
**	mov	w2, 2
**	mov	w1, 1
**	mov	w0, 0
**	bl	aarchpcs_overflow 
**	...
*/

void
aarchpcs_overflow_call (void)
{
  Empty e;
  aarchpcs_overflow (0, 1, 2, 3, 4, 5, 6, 7, e);
}

__attribute__ ((__noinline__, preserve_none)) void
preserve_none_overflow (int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, Empty);

/*
**preserve_none_overflow_call:
**	...
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
**	...
**	bl	preserve_none_overflow 
**	...
*/

__attribute__ ((preserve_none)) void
preserve_none_overflow_call (void)
{
  Empty e;
  preserve_none_overflow(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, e);
}

