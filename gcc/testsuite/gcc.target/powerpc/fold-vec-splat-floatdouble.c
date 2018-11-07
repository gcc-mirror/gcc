/* Verify that overloaded built-ins for vec_splat with float and
   double inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

/* Floats.  */
vector float testf_00 (vector float x) { return vec_splat (x, 0b00000); }
vector float testf_01 (vector float x) { return vec_splat (x, 0b00001); }
vector float testf_02 (vector float x) { return vec_splat (x, 0b00010); }
vector float test_fc ()
{ const vector float y = { 7.1, 8.2, 9.3, 10.4}; return vec_splat (y, 0b00010); }

/* Doubles.  */
vector double testd_00 (vector double x) { return vec_splat (x, 0b00000); }
vector double testd_01 (vector double x) { return vec_splat (x, 0b00001); }
vector double test_dc ()
{ const vector double y = { 3.0, 5.0 }; return vec_splat (y, 0b00010); }

/* If the source vector is a known constant, we will generate a load.  */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M|\mlxv\M} 2 } } */

/* For float types, we generate a splat.  */
/* { dg-final { scan-assembler-times "vspltw|xxspltw" 3 } } */

/* For double types, we will generate xxpermdi instructions.  */
/* { dg-final { scan-assembler-times "xxpermdi" 3 } } */

