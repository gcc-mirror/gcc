/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

vector float
vcefb (vector signed int a)
{
  return vec_float (a);
}

/* { dg-final { scan-assembler-times "vcefb.*\n\tvcefb.*floatv4siv4sf2" 1 } } */

vector float
vcelfb (vector unsigned int a)
{
  return vec_float (a);
}

/* { dg-final { scan-assembler-times "vcelfb.*\n\tvcelfb.*floatunsv4siv4sf2" 1 } } */

vector float
vcefb_mem (vector signed int *a)
{
  return vec_float (*a);
}

vector float
vcelfb_mem (vector unsigned int *a)
{
  return vec_float (*a);
}

/* The following immediates are being converted and directly stored
   in the literal pool so no explicit conversion is necessary.   */
/* { dg-final { scan-assembler-times "vl\t%v\[0-9\]+,\.L\[0-9\]+\-\.L\[0-9\]+\\(%r\[0-9\]+\\)" 2 } } */

vector float
vcefb_imm ()
{
  return vec_float ((vector signed int) { 1, -2 });
}

vector float
vcelfb_imm ()
{
  return vec_float ((vector unsigned int){ 1, 2 });
}

/* { dg-final { scan-assembler-times "vcefb\t" 2 } } */
/* { dg-final { scan-assembler-times "vcelfb\t" 2 } } */
