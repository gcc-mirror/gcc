/* Verify that overloaded built-ins for vec_splat with pixel
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector pixel test1_00 (vector pixel x) { return vec_splat (x, 0b00000); }
vector pixel test1_01 (vector pixel x) { return vec_splat (x, 0b00001); }
vector pixel test1_02 (vector pixel x) { return vec_splat (x, 0b00010); }
vector pixel test1_04 (vector pixel x) { return vec_splat (x, 0b00100); }
vector pixel test1_08 (vector pixel x) { return vec_splat (x, 0b01000); }
vector pixel test1_10 (vector pixel x) { return vec_splat (x, 0b10000); }
vector pixel test1_1e (vector pixel x) { return vec_splat (x, 0b11110); }
vector pixel test1_1f (vector pixel x) { return vec_splat (x, 0b11111); }

/* Similar test as above, but the source vector is a known constant. */
vector pixel test_p () { const vector pixel y = { 1,2,3,4}; return vec_splat (y, 0b00010); }

/* Similar tests as above, mask is greater than number of elements in the
 source vector.  */
vector pixel test_op () { const vector pixel y = { 1,2,3,4}; return vec_splat (y, 0b10010); }

/* { dg-final { scan-assembler-times "vspltish" 2 } } */
/* { dg-final { scan-assembler-times "vsplth" 8 } } */
