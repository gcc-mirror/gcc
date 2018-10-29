/* Verify that overloaded built-ins for vec_splat with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool int testb_0  (vector bool int x) { return vec_splat (x, 0b00000); }
vector bool int testb_1  (vector bool int x) { return vec_splat (x, 0b00001); }
vector bool int testb_2  (vector bool int x) { return vec_splat (x, 0b00010); }

vector signed int tests_0  (vector signed int x) { return vec_splat (x, 0b00000); }
vector signed int tests_1  (vector signed int x) { return vec_splat (x, 0b00001); }
vector signed int tests_2  (vector signed int x) { return vec_splat (x, 0b00010); }

vector unsigned int testu_0  (vector unsigned int x) { return vec_splat (x, 0b00000); }
vector unsigned int testu_1  (vector unsigned int x) { return vec_splat (x, 0b00001); }
vector unsigned int testu_2  (vector unsigned int x) { return vec_splat (x, 0b00010); }

/* Similar test as above, but the source vector is a known constant. */
vector bool int test_bic () { const vector bool int y = { 1,2,3,4}; return vec_splat (y, 0b00010); }
vector signed int test_sic () { const vector signed int y = { 1,2,3,4}; return vec_splat (y, 0b00010); }
vector unsigned int test_uic () { const vector unsigned int y = { 1,2,3,4}; return vec_splat (y, 0b00010); }

/* { dg-final { scan-assembler-times "vspltisw" 3 } } */
/* { dg-final { scan-assembler-times "vspltw|xxspltw" 9 } } */

