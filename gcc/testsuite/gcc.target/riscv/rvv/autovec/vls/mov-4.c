/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh_zvl4096b -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** mov:
**	lw\s+[a-x0-9]+,0\s*\([a-x0-9]+\)
**	lw\s+[a-x0-9]+,4\s*\([a-x0-9]+\)
**	sw\s+[a-x0-9]+,0\s*\([a-x0-9]+\)
**	sw\s+[a-x0-9]+,4\s*\([a-x0-9]+\)
**  ret
*/
void mov (int16_t *in, int16_t *out)
{
  v4hi v = *(v4hi*)in;
  *(v4hi*)out = v;
}
