/* { dg-do compile } */
/* { dg-options "-march=rv32imafc -mabi=ilp32f -msave-restore -O2 -fno-schedule-insns -fno-schedule-insns2 -fno-unroll-loops -fno-peel-loops" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

char my_getchar();
float getf();

/*
** bar:
**	call	t0,__riscv_save_[0-9]+
**	addi	sp,sp,-[0-9]+
**	...
**	li	t0,-[0-9]+
**	add	sp,sp,t0
**	...
**	li	t0,[0-9]+
**	add	sp,sp,t0
**	...
**	addi	sp,sp,[0-9]+
**	tail	__riscv_restore_[0-9]+
*/
int bar()
{
  float volatile farray[3568];

  float sum = 0;
  float f1 = getf();
  float f2 = getf();
  float f3 = getf();
  float f4 = getf();

  for (int i = 0; i < 3568; i++)
  {
    farray[i] = my_getchar() * 1.2;
    sum += farray[i];
  }

  return sum + f1 + f2 + f3 + f4;
}

