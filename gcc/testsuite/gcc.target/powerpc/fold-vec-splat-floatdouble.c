/* Verify that overloaded built-ins for vec_splat with float and
   double inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector float testf_00 (vector float x) { return vec_splat (x, 0b00000); }
vector float testf_01 (vector float x) { return vec_splat (x, 0b00001); }
vector float testf_02 (vector float x) { return vec_splat (x, 0b00010); }
vector float testf_04 (vector float x) { return vec_splat (x, 0b00100); }
vector float testf_08 (vector float x) { return vec_splat (x, 0b01000); }
vector float testf_0f (vector float x) { return vec_splat (x, 0b01111); }
vector float testf_10 (vector float x) { return vec_splat (x, 0b10000); }
vector float testf_1e (vector float x) { return vec_splat (x, 0b11110); }
vector float testf_1f (vector float x) { return vec_splat (x, 0b11111); }

vector double testd_00 (vector double x) { return vec_splat (x, 0b00000); }
vector double testd_01 (vector double x) { return vec_splat (x, 0b00001); }
vector double testd_02 (vector double x) { return vec_splat (x, 0b00010); }
vector double testd_04 (vector double x) { return vec_splat (x, 0b00100); }
vector double testd_08 (vector double x) { return vec_splat (x, 0b01000); }
vector double testd_0f (vector double x) { return vec_splat (x, 0b01111); }
vector double testd_10 (vector double x) { return vec_splat (x, 0b10000); }
vector double testd_1e (vector double x) { return vec_splat (x, 0b11110); }
vector double testd_1f (vector double x) { return vec_splat (x, 0b11111); }

/* Similar tests as above, but the source vector is a known constant. */
vector float test_fc () { const vector float y = { 7.1, 8.2, 9.3, 10.4}; return vec_splat (y, 0b00010); }
vector double test_dc () { const vector double y = { 3.0, 5.0 }; return vec_splat (y, 0b00010); }

/* Similar tests as above, mask is greater than number of elements in the
 source vector.  */
vector float test_ofc () { const vector float y = { 7.1, 8.2, 9.3, 10.4}; return vec_splat (y, 0b10010); }
vector double test_odc () { const vector double y = { 3.0, 5.0 }; return vec_splat (y, 0b10010); }

/* lvx or lxvd2x for loading of the constants.  */
/* vspltw or xxspltw for non-constants with the float type.  */
/* xxpermdi for non-constants with the double type.  */

/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M} 4 } } */
/* { dg-final { scan-assembler-times "vspltw|xxspltw" 9 } } */
/* { dg-final { scan-assembler-times "xxpermdi" 9 } } */

