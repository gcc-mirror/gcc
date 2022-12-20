/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include <stddef.h>
#include "riscv_vector.h"

size_t test_vsetvl_e8mf8_imm0()
{
  size_t vl = __riscv_vsetvl_e8mf8(0);
  return vl;
}

size_t test_vsetvl_e8mf8_imm31()
{
  size_t vl = __riscv_vsetvl_e8mf8(31);
  return vl;
}

size_t test_vsetvl_e8mf8_imm32()
{
  size_t vl = __riscv_vsetvl_e8mf8(32);
  return vl;
}

size_t test_vsetvl_e8mf8(size_t avl)
{
  size_t vl = __riscv_vsetvl_e8mf8(avl);
  return vl;
}

size_t test_vsetvlmax_e8mf8()
{
  size_t vl = __riscv_vsetvlmax_e8mf8();
  return vl;
}

size_t test_vsetvl_e8mf4_imm0()
{
  size_t vl = __riscv_vsetvl_e8mf4(0);
  return vl;
}

size_t test_vsetvl_e8mf4_imm31()
{
  size_t vl = __riscv_vsetvl_e8mf4(31);
  return vl;
}

size_t test_vsetvl_e8mf4_imm32()
{
  size_t vl = __riscv_vsetvl_e8mf4(32);
  return vl;
}

size_t test_vsetvl_e8mf4(size_t avl)
{
  size_t vl = __riscv_vsetvl_e8mf4(avl);
  return vl;
}

size_t test_vsetvlmax_e8mf4()
{
  size_t vl = __riscv_vsetvlmax_e8mf4();
  return vl;
}

size_t test_vsetvl_e8mf2_imm0()
{
  size_t vl = __riscv_vsetvl_e8mf2(0);
  return vl;
}

size_t test_vsetvl_e8mf2_imm31()
{
  size_t vl = __riscv_vsetvl_e8mf2(31);
  return vl;
}

size_t test_vsetvl_e8mf2_imm32()
{
  size_t vl = __riscv_vsetvl_e8mf2(32);
  return vl;
}

size_t test_vsetvl_e8mf2(size_t avl)
{
  size_t vl = __riscv_vsetvl_e8mf2(avl);
  return vl;
}

size_t test_vsetvlmax_e8mf2()
{
  size_t vl = __riscv_vsetvlmax_e8mf2();
  return vl;
}

size_t test_vsetvl_e8m1_imm0()
{
  size_t vl = __riscv_vsetvl_e8m1(0);
  return vl;
}

size_t test_vsetvl_e8m1_imm31()
{
  size_t vl = __riscv_vsetvl_e8m1(31);
  return vl;
}

size_t test_vsetvl_e8m1_imm32()
{
  size_t vl = __riscv_vsetvl_e8m1(32);
  return vl;
}

size_t test_vsetvl_e8m1(size_t avl)
{
  size_t vl = __riscv_vsetvl_e8m1(avl);
  return vl;
}

size_t test_vsetvlmax_e8m1()
{
  size_t vl = __riscv_vsetvlmax_e8m1();
  return vl;
}

size_t test_vsetvl_e8m2_imm0()
{
  size_t vl = __riscv_vsetvl_e8m2(0);
  return vl;
}

size_t test_vsetvl_e8m2_imm31()
{
  size_t vl = __riscv_vsetvl_e8m2(31);
  return vl;
}

size_t test_vsetvl_e8m2_imm32()
{
  size_t vl = __riscv_vsetvl_e8m2(32);
  return vl;
}

size_t test_vsetvl_e8m2(size_t avl)
{
  size_t vl = __riscv_vsetvl_e8m2(avl);
  return vl;
}

size_t test_vsetvlmax_e8m2()
{
  size_t vl = __riscv_vsetvlmax_e8m2();
  return vl;
}

size_t test_vsetvl_e8m4_imm0()
{
  size_t vl = __riscv_vsetvl_e8m4(0);
  return vl;
}

size_t test_vsetvl_e8m4_imm31()
{
  size_t vl = __riscv_vsetvl_e8m4(31);
  return vl;
}

size_t test_vsetvl_e8m4_imm32()
{
  size_t vl = __riscv_vsetvl_e8m4(32);
  return vl;
}
size_t test_vsetvl_e8m4(size_t avl)
{
  size_t vl = __riscv_vsetvl_e8m4(avl);
  return vl;
}

size_t test_vsetvlmax_e8m4()
{
  size_t vl = __riscv_vsetvlmax_e8m4();
  return vl;
}

size_t test_vsetvl_e8m8_imm0()
{
  size_t vl = __riscv_vsetvl_e8m8(0);
  return vl;
}

size_t test_vsetvl_e8m8_imm31()
{
  size_t vl = __riscv_vsetvl_e8m8(31);
  return vl;
}

size_t test_vsetvl_e8m8_imm32()
{
  size_t vl = __riscv_vsetvl_e8m8(32);
  return vl;
}

size_t test_vsetvl_e8m8(size_t avl)
{
  size_t vl = __riscv_vsetvl_e8m8(avl);
  return vl;
}

size_t test_vsetvlmax_e8m8()
{
  size_t vl = __riscv_vsetvlmax_e8m8();
  return vl;
}

size_t test_vsetvl_e16mf4_imm0()
{
  size_t vl = __riscv_vsetvl_e16mf4(0);
  return vl;
}

size_t test_vsetvl_e16mf4_imm31()
{
  size_t vl = __riscv_vsetvl_e16mf4(31);
  return vl;
}

size_t test_vsetvl_e16mf4_imm32()
{
  size_t vl = __riscv_vsetvl_e16mf4(32);
  return vl;
}

size_t test_vsetvl_e16mf4(size_t avl)
{
  size_t vl = __riscv_vsetvl_e16mf4(avl);
  return vl;
}

size_t test_vsetvlmax_e16mf4()
{
  size_t vl = __riscv_vsetvlmax_e16mf4();
  return vl;
}

size_t test_vsetvl_e16mf2_imm0()
{
  size_t vl = __riscv_vsetvl_e16mf2(0);
  return vl;
}

size_t test_vsetvl_e16mf2_imm31()
{
  size_t vl = __riscv_vsetvl_e16mf2(31);
  return vl;
}

size_t test_vsetvl_e16mf2_imm32()
{
  size_t vl = __riscv_vsetvl_e16mf2(32);
  return vl;
}

size_t test_vsetvl_e16mf2(size_t avl)
{
  size_t vl = __riscv_vsetvl_e16mf2(avl);
  return vl;
}

size_t test_vsetvlmax_e16mf2()
{
  size_t vl = __riscv_vsetvlmax_e16mf2();
  return vl;
}

size_t test_vsetvl_e16m1_imm0()
{
  size_t vl = __riscv_vsetvl_e16m1(0);
  return vl;
}

size_t test_vsetvl_e16m1_imm31()
{
  size_t vl = __riscv_vsetvl_e16m1(31);
  return vl;
}

size_t test_vsetvl_e16m1_imm32()
{
  size_t vl = __riscv_vsetvl_e16m1(32);
  return vl;
}

size_t test_vsetvl_e16m1(size_t avl)
{
  size_t vl = __riscv_vsetvl_e16m1(avl);
  return vl;
}

size_t test_vsetvlmax_e16m1()
{
  size_t vl = __riscv_vsetvlmax_e16m1();
  return vl;
}

size_t test_vsetvl_e16m2_imm0()
{
  size_t vl = __riscv_vsetvl_e16m2(0);
  return vl;
}

size_t test_vsetvl_e16m2_imm31()
{
  size_t vl = __riscv_vsetvl_e16m2(31);
  return vl;
}

size_t test_vsetvl_e16m2_imm32()
{
  size_t vl = __riscv_vsetvl_e16m2(32);
  return vl;
}

size_t test_vsetvl_e16m2(size_t avl)
{
  size_t vl = __riscv_vsetvl_e16m2(avl);
  return vl;
}

size_t test_vsetvlmax_e16m2()
{
  size_t vl = __riscv_vsetvlmax_e16m2();
  return vl;
}

size_t test_vsetvl_e16m4_imm0()
{
  size_t vl = __riscv_vsetvl_e16m4(0);
  return vl;
}

size_t test_vsetvl_e16m4_imm31()
{
  size_t vl = __riscv_vsetvl_e16m4(31);
  return vl;
}

size_t test_vsetvl_e16m4_imm32()
{
  size_t vl = __riscv_vsetvl_e16m4(32);
  return vl;
}

size_t test_vsetvl_e16m4(size_t avl)
{
  size_t vl = __riscv_vsetvl_e16m4(avl);
  return vl;
}

size_t test_vsetvlmax_e16m4()
{
  size_t vl = __riscv_vsetvlmax_e16m4();
  return vl;
}

size_t test_vsetvl_e16m8_imm0()
{
  size_t vl = __riscv_vsetvl_e16m8(0);
  return vl;
}

size_t test_vsetvl_e16m8_imm31()
{
  size_t vl = __riscv_vsetvl_e16m8(31);
  return vl;
}

size_t test_vsetvl_e16m8_imm32()
{
  size_t vl = __riscv_vsetvl_e16m8(32);
  return vl;
}

size_t test_vsetvl_e16m8(size_t avl)
{
  size_t vl = __riscv_vsetvl_e16m8(avl);
  return vl;
}

size_t test_vsetvlmax_e16m8()
{
  size_t vl = __riscv_vsetvlmax_e16m8();
  return vl;
}

size_t test_vsetvl_e32mf2_imm0()
{
  size_t vl = __riscv_vsetvl_e32mf2(0);
  return vl;
}

size_t test_vsetvl_e32mf2_imm31()
{
  size_t vl = __riscv_vsetvl_e32mf2(31);
  return vl;
}

size_t test_vsetvl_e32mf2_imm32()
{
  size_t vl = __riscv_vsetvl_e32mf2(32);
  return vl;
}

size_t test_vsetvl_e32mf2(size_t avl)
{
  size_t vl = __riscv_vsetvl_e32mf2(avl);
  return vl;
}

size_t test_vsetvlmax_e32mf2()
{
  size_t vl = __riscv_vsetvlmax_e32mf2();
  return vl;
}

size_t test_vsetvl_e32m1_imm0()
{
  size_t vl = __riscv_vsetvl_e32m1(0);
  return vl;
}

size_t test_vsetvl_e32m1_imm31()
{
  size_t vl = __riscv_vsetvl_e32m1(31);
  return vl;
}

size_t test_vsetvl_e32m1_imm32()
{
  size_t vl = __riscv_vsetvl_e32m1(32);
  return vl;
}

size_t test_vsetvl_e32m1(size_t avl)
{
  size_t vl = __riscv_vsetvl_e32m1(avl);
  return vl;
}

size_t test_vsetvlmax_e32m1()
{
  size_t vl = __riscv_vsetvlmax_e32m1();
  return vl;
}

size_t test_vsetvl_e32m2_imm0()
{
  size_t vl = __riscv_vsetvl_e32m2(0);
  return vl;
}

size_t test_vsetvl_e32m2_imm31()
{
  size_t vl = __riscv_vsetvl_e32m2(31);
  return vl;
}

size_t test_vsetvl_e32m2_imm32()
{
  size_t vl = __riscv_vsetvl_e32m2(32);
  return vl;
}

size_t test_vsetvl_e32m2(size_t avl)
{
  size_t vl = __riscv_vsetvl_e32m2(avl);
  return vl;
}

size_t test_vsetvlmax_e32m2()
{
  size_t vl = __riscv_vsetvlmax_e32m2();
  return vl;
}

size_t test_vsetvl_e32m4_imm0()
{
  size_t vl = __riscv_vsetvl_e32m4(0);
  return vl;
}

size_t test_vsetvl_e32m4_imm31()
{
  size_t vl = __riscv_vsetvl_e32m4(31);
  return vl;
}

size_t test_vsetvl_e32m4_imm32()
{
  size_t vl = __riscv_vsetvl_e32m4(32);
  return vl;
}

size_t test_vsetvl_e32m4(size_t avl)
{
  size_t vl = __riscv_vsetvl_e32m4(avl);
  return vl;
}

size_t test_vsetvlmax_e32m4()
{
  size_t vl = __riscv_vsetvlmax_e32m4();
  return vl;
}

size_t test_vsetvl_e32m8_imm0()
{
  size_t vl = __riscv_vsetvl_e32m8(0);
  return vl;
}

size_t test_vsetvl_e32m8_imm31()
{
  size_t vl = __riscv_vsetvl_e32m8(31);
  return vl;
}

size_t test_vsetvl_e32m8_imm32()
{
  size_t vl = __riscv_vsetvl_e32m8(32);
  return vl;
}
size_t test_vsetvl_e32m8(size_t avl)
{
  size_t vl = __riscv_vsetvl_e32m8(avl);
  return vl;
}

size_t test_vsetvlmax_e32m8()
{
  size_t vl = __riscv_vsetvlmax_e32m8();
  return vl;
}

size_t test_vsetvl_e64m1_imm0()
{
  size_t vl = __riscv_vsetvl_e64m1(0);
  return vl;
}

size_t test_vsetvl_e64m1_imm31()
{
  size_t vl = __riscv_vsetvl_e64m1(31);
  return vl;
}

size_t test_vsetvl_e64m1_imm32()
{
  size_t vl = __riscv_vsetvl_e64m1(32);
  return vl;
}

size_t test_vsetvl_e64m1(size_t avl)
{
  size_t vl = __riscv_vsetvl_e64m1(avl);
  return vl;
}

size_t test_vsetvlmax_e64m1()
{
  size_t vl = __riscv_vsetvlmax_e64m1();
  return vl;
}

size_t test_vsetvl_e64m2_imm0()
{
  size_t vl = __riscv_vsetvl_e64m2(0);
  return vl;
}

size_t test_vsetvl_e64m2_imm31()
{
  size_t vl = __riscv_vsetvl_e64m2(31);
  return vl;
}

size_t test_vsetvl_e64m2_imm32()
{
  size_t vl = __riscv_vsetvl_e64m2(32);
  return vl;
}
size_t test_vsetvl_e64m2(size_t avl)
{
  size_t vl = __riscv_vsetvl_e64m2(avl);
  return vl;
}

size_t test_vsetvlmax_e64m2()
{
  size_t vl = __riscv_vsetvlmax_e64m2();
  return vl;
}

size_t test_vsetvl_e64m4_imm0()
{
  size_t vl = __riscv_vsetvl_e64m4(0);
  return vl;
}

size_t test_vsetvl_e64m4_imm31()
{
  size_t vl = __riscv_vsetvl_e64m4(31);
  return vl;
}

size_t test_vsetvl_e64m4_imm32()
{
  size_t vl = __riscv_vsetvl_e64m4(32);
  return vl;
}

size_t test_vsetvl_e64m4(size_t avl)
{
  size_t vl = __riscv_vsetvl_e64m4(avl);
  return vl;
}

size_t test_vsetvlmax_e64m4()
{
  size_t vl = __riscv_vsetvlmax_e64m4();
  return vl;
}

size_t test_vsetvl_e64m8_imm0()
{
  size_t vl = __riscv_vsetvl_e64m8(0);
  return vl;
}

size_t test_vsetvl_e64m8_imm31()
{
  size_t vl = __riscv_vsetvl_e64m8(31);
  return vl;
}

size_t test_vsetvl_e64m8_imm32()
{
  size_t vl = __riscv_vsetvl_e64m8(32);
  return vl;
}
size_t test_vsetvl_e64m8(size_t avl)
{
  size_t vl = __riscv_vsetvl_e64m8(avl);
  return vl;
}

size_t test_vsetvlmax_e64m8()
{
  size_t vl = __riscv_vsetvlmax_e64m8();
  return vl;
}

/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e8,\s*mf8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e8,\s*mf8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e8,\s*mf4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e8,\s*mf4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e8,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e8,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e8,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e8,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*m1,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e8,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e8,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*m2,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e8,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e8,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*m4,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e8,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e8,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*m8,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e16,\s*mf4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e16,\s*mf4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e16,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e16,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e16,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e16,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e16,\s*m1,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e16,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e16,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e16,\s*m2,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e16,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e16,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e16,\s*m4,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e16,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e16,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e16,\s*m8,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e32,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e32,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e32,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e32,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e32,\s*m1,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e32,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e32,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e32,\s*m2,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e32,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e32,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e32,\s*m4,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e32,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e32,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e32,\s*m8,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e64,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e64,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e64,\s*m1,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e64,\s*m1,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e64,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e64,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e64,\s*m2,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e64,\s*m2,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e64,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e64,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e64,\s*m4,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e64,\s*m4,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*0,\s*e64,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*31,\s*e64,\s*m8,\s*ta,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e64,\s*m8,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e64,\s*m8,\s*ta,\s*mu} 1 } } */
