/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "RTL input" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fno-schedule-fusion -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Disabling the pass preserves the original order.  */
/*
**sched_fusion_load_pair:
**	ld	a1,0\(a0\)
**	add	a3,a4,a5
**	ld	a2,8\(a0\)
**	ret
*/
#include "sched-fusion-priority.c"
