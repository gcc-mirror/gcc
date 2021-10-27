/* { dg-do compile } */
/* Specify C99 to avoid the warning/error on compound literals.  */
/* { dg-options "-O1 -std=c99 -Wno-psabi" } */

/* Verify that there is no ICE.  */

typedef __attribute__((vector_size(8))) signed char int8x8_t;
typedef __attribute__((vector_size(8))) unsigned char uint8x8_t;

int8x8_t fn1 (int8x8_t val20, char tmp)
{
  uint8x8_t __trans_tmp_3;
  __trans_tmp_3 = (uint8x8_t){tmp};
  int8x8_t __a = (int8x8_t) __trans_tmp_3;
  return __builtin_shuffle (__a, val20, (uint8x8_t){0});
}
