/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fno-vect-cost-model" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define F double
#define I long long

#include "vfcmp-f.c"

/*
** compare_quiet_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.ceq.d	(\$vr[0-9]+),(\1,\2|\2,\1)
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_not_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cune.d	(\$vr[0-9]+),(\1,\2|\2,\1)
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_greater:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.slt.d	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_greater_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sle.d	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_less:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.slt.d	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_less_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sle.d	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_not_greater:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sule.d	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_less_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sult.d	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_not_less:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sule.d	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_greater_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sult.d	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_less:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.clt.d	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_less_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cle.d	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_greater:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.clt.d	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_greater_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cle.d	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_not_less:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cule.d	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_greater_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cult.d	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_not_greater:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cule.d	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_less_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cult.d	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cun.d	(\$vr[0-9]+),(\1,\2|\2,\1)
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_ordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cor.d	(\$vr[0-9]+),(\1,\2|\2,\1)
**	vst	\3,\$r6,0
**	jr	\$r1
*/
