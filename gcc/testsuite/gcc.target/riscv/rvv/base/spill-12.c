/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -msave-restore -fno-schedule-insns -fno-schedule-insns2 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */


void fn2 (float a1, float a2, float a3, float a4,
          float a5, float a6, float a7, float a8);
void fn3 (char*);


/*
** stack_save_restore_1:
**	call\tt0,__riscv_save_0
**	li\tt0,-8192
**	addi\tt0,t0,192
**	add\tsp,sp,t0
**	...
**	mv\ta0,sp
**	...
**	tail\t__riscv_restore_0
*/
int stack_save_restore_1 (float a1, float a2, float a3, float a4,
                      float a5, float a6, float a7, float a8)
{
  char d[8000];
  float f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13;
  asm volatile ("nop"
                : "=f" (f1), "=f" (f2), "=f" (f3), "=f" (f4), "=f" (f5), "=f" (f6),
                  "=f" (f7), "=f" (f8), "=f" (f9),  "=f" (f10), "=f" (f11),
                  "=f" (f12), "=f" (f13)
                :
                :);
  asm volatile ("nop"
                :
                : "f" (f1), "f" (f2), "f" (f3), "f" (f4), "f" (f5), "f" (f6),
                  "f" (f7), "f" (f8), "f" (f9), "f" (f10), "f" (f11),
                  "f" (f12), "f" (f13)
                :);
  fn2 (a1, a2, a3, a4, a5, a6, a7, a8);
  fn3(d);
  return 0;
}
