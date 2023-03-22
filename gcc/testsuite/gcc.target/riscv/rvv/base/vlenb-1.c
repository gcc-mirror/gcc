/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void f0 (char *x, char * p1, char * p2, char * p3) {
  vbool32_t bp1 = *(vbool32_t*)p1;
  vbool32_t bp2 = *(vbool32_t*)p2;
  vbool32_t bp3 = *(vbool32_t*)p3;
  asm volatile ("":::"memory");
  *(vbool32_t *)(x + (__riscv_vlenb())) = bp2;  
  *(vbool32_t *)(x) = bp1;             
  *(vbool32_t *)(x + (__riscv_vlenb())*2) = bp3;
}

void f1 (char *x, char * p1, char * p2, char * p3) {
  vbool32_t bp1 = *(vbool32_t*)p1;
  vbool32_t bp2 = *(vbool32_t*)p2;
  vbool32_t bp3 = *(vbool32_t*)p3;
  asm volatile ("":::"memory");
  *(vbool32_t *)(x + (__riscv_vlenb() / 2)) = bp2;  
  *(vbool32_t *)(x) = bp1;             
  *(vbool32_t *)(x + (__riscv_vlenb() / 2)*2) = bp3;
}

void f2 (char *x, char * p1, char * p2, char * p3) {
  vbool32_t bp1 = *(vbool32_t*)p1;
  vbool32_t bp2 = *(vbool32_t*)p2;
  vbool32_t bp3 = *(vbool32_t*)p3;
  asm volatile ("":::"memory");
  *(vbool32_t *)(x + (__riscv_vlenb() / 4)) = bp2;  
  *(vbool32_t *)(x) = bp1;             
  *(vbool32_t *)(x + (__riscv_vlenb() / 4)*2) = bp3;
}

void f3 (char *x, char * p1, char * p2, char * p3) {
  vbool32_t bp1 = *(vbool32_t*)p1;
  vbool32_t bp2 = *(vbool32_t*)p2;
  vbool32_t bp3 = *(vbool32_t*)p3;
  asm volatile ("":::"memory");
  *(vbool32_t *)(x + (__riscv_vlenb() / 4)) = bp2;  
  *(vbool32_t *)(x) = bp1;             
  *(vbool32_t *)(x + (__riscv_vlenb() / 4)*2) = bp3;
}

/* { dg-final { scan-assembler-times {vsm\.v} 12 } } */
