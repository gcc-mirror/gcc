/* { dg-do compile } */

/* Checks from the original ops.c that pass pointers to long or
   unsigned long to operations that do not support that in released
   versions of altivec.h.  */

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

void f36() {
  vec_st(var_vec_s32[0], var_int[1], var_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_st(var_vec_u32[0], var_int[1], var_unsigned_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
}
void f37() {
  vec_ste(var_vec_s32[0], var_int[1], var_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_ste(var_vec_u32[0], var_int[1], var_unsigned_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_stl(var_vec_s32[0], var_int[1], var_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_stl(var_vec_u32[0], var_int[1], var_unsigned_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_stvewx(var_vec_s32[0], var_int[1], var_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_stvewx(var_vec_u32[0], var_int[1], var_unsigned_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_stvx(var_vec_s32[0], var_int[1], var_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_stvx(var_vec_u32[0], var_int[1], var_unsigned_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_stvxl(var_vec_s32[0], var_int[1], var_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
  vec_stvxl(var_vec_u32[0], var_int[1], var_unsigned_long_ptr[2]); /* { dg-error "invalid parameter combination" } */
}
