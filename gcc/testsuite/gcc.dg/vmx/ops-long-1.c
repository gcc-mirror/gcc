/* { dg-do compile } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mno-vsx -Wno-deprecated" } */

/* Checks from the original ops.c that pass pointers to long or
   unsigned long for operations that support that in released versions
   of <altivec.h>.  */

#include <altivec.h>
#include <stdlib.h>
extern int *var_int;
extern long * *var_long_ptr;
extern unsigned long * *var_unsigned_long_ptr;
extern vector signed int * *var_vec_s32_ptr;
extern vector signed int *var_vec_s32;
extern vector unsigned char * *var_vec_u8_ptr;
extern vector unsigned char *var_vec_u8;
extern vector unsigned int * *var_vec_u32_ptr;
extern vector unsigned int *var_vec_u32;

void f13() {
  *var_vec_s32++ = vec_ld(var_int[0], var_long_ptr[1]);
  *var_vec_s32++ = vec_lde(var_int[0], var_long_ptr[1]);
  *var_vec_s32++ = vec_ldl(var_int[0], var_long_ptr[1]);
  *var_vec_s32++ = vec_lvewx(var_int[0], var_long_ptr[1]);
  *var_vec_s32++ = vec_lvx(var_int[0], var_long_ptr[1]);
  *var_vec_s32++ = vec_lvxl(var_int[0], var_long_ptr[1]);
}
void f22() {
  *var_vec_u32++ = vec_ld(var_int[0], var_unsigned_long_ptr[1]);
  *var_vec_u32++ = vec_lde(var_int[0], var_unsigned_long_ptr[1]);
  *var_vec_u32++ = vec_ldl(var_int[0], var_unsigned_long_ptr[1]);
  *var_vec_u32++ = vec_lvewx(var_int[0], var_unsigned_long_ptr[1]);
  *var_vec_u32++ = vec_lvx(var_int[0], var_unsigned_long_ptr[1]);
  *var_vec_u32++ = vec_lvxl(var_int[0], var_unsigned_long_ptr[1]);
}
void f25() {
  *var_vec_u8++ = vec_lvsl(var_int[0], var_long_ptr[1]);
  *var_vec_u8++ = vec_lvsl(var_int[0], var_unsigned_long_ptr[1]);
  *var_vec_u8++ = vec_lvsr(var_int[0], var_long_ptr[1]);
  *var_vec_u8++ = vec_lvsr(var_int[0], var_unsigned_long_ptr[1]);
}
void f33() {
  vec_dst(var_long_ptr[0], var_int[1], 0);
  vec_dst(var_long_ptr[0], var_int[1], 1);
  vec_dst(var_long_ptr[0], var_int[1], 2);
  vec_dst(var_long_ptr[0], var_int[1], 3);
  vec_dst(var_unsigned_long_ptr[0], var_int[1], 0);
  vec_dst(var_unsigned_long_ptr[0], var_int[1], 1);
  vec_dst(var_unsigned_long_ptr[0], var_int[1], 2);
  vec_dst(var_unsigned_long_ptr[0], var_int[1], 3);
}
void f34() {
  vec_dstst(var_long_ptr[0], var_int[1], 0);
  vec_dstst(var_long_ptr[0], var_int[1], 1);
  vec_dstst(var_long_ptr[0], var_int[1], 2);
  vec_dstst(var_long_ptr[0], var_int[1], 3);
  vec_dstst(var_unsigned_long_ptr[0], var_int[1], 0);
  vec_dstst(var_unsigned_long_ptr[0], var_int[1], 1);
  vec_dstst(var_unsigned_long_ptr[0], var_int[1], 2);
  vec_dstst(var_unsigned_long_ptr[0], var_int[1], 3);
}
void f35() {
  vec_dststt(var_long_ptr[0], var_int[1], 0);
  vec_dststt(var_long_ptr[0], var_int[1], 1);
  vec_dststt(var_long_ptr[0], var_int[1], 2);
  vec_dststt(var_long_ptr[0], var_int[1], 3);
  vec_dststt(var_unsigned_long_ptr[0], var_int[1], 0);
  vec_dststt(var_unsigned_long_ptr[0], var_int[1], 1);
  vec_dststt(var_unsigned_long_ptr[0], var_int[1], 2);
  vec_dststt(var_unsigned_long_ptr[0], var_int[1], 3);
  vec_dstt(var_long_ptr[0], var_int[1], 0);
  vec_dstt(var_long_ptr[0], var_int[1], 1);
  vec_dstt(var_long_ptr[0], var_int[1], 2);
  vec_dstt(var_long_ptr[0], var_int[1], 3);
}
void f36() {
  vec_dstt(var_unsigned_long_ptr[0], var_int[1], 0);
  vec_dstt(var_unsigned_long_ptr[0], var_int[1], 1);
  vec_dstt(var_unsigned_long_ptr[0], var_int[1], 2);
  vec_dstt(var_unsigned_long_ptr[0], var_int[1], 3);
}
