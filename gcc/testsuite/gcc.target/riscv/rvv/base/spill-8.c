/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

void f2 (char*);
void f3 (char*, ...);

/*
** stach_check_alloca_1:
**	addi	sp,sp,-48
**	sw	ra,12\(sp\)
**	sw	s0,8\(sp\)
**	addi	s0,sp,16
**	...
**	addi	a0,a0,23
**	andi	a0,a0,-16
**	sub	sp,sp,a0
**	...
**	addi	sp,s0,-16
**	lw	ra,12\(sp\)
**	lw	s0,8\(sp\)
**	addi	sp,sp,48
**	jr	ra
*/
void stach_check_alloca_1 (int y, ...)
{
  char* pStr = (char*)__builtin_alloca(y);
  f2(pStr);
}

/*
** stach_check_alloca_2:
**	addi	sp,sp,-48
**	sw	ra,44\(sp\)
**	sw	s0,40\(sp\)
**	addi	s0,sp,48
**	addi	a0,a0,23
**	andi	a0,a0,-16
**	sub	sp,sp,a0
**	...
**	addi	sp,s0,-48
**	lw	ra,44\(sp\)
**	lw	s0,40\(sp\)
**	addi	sp,sp,48
**	jr	ra
*/
void stach_check_alloca_2 (int y)
{
  char* pStr = (char*)__builtin_alloca(y);
  f3(pStr, pStr, pStr, pStr, pStr, pStr, pStr, pStr, 2, pStr, pStr, pStr, 1);
}
