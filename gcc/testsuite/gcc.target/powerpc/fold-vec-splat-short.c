/* Verify that overloaded built-ins for vec_splat with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool short testb_00 (vector bool short x) { return vec_splat (x, 0b00000); }
vector bool short testb_01 (vector bool short x) { return vec_splat (x, 0b00001); }
vector bool short testb_02 (vector bool short x) { return vec_splat (x, 0b00010); }
vector bool short testb_04 (vector bool short x) { return vec_splat (x, 0b00100); }
vector bool short testb_08 (vector bool short x) { return vec_splat (x, 0b01000); }
vector bool short testb_10 (vector bool short x) { return vec_splat (x, 0b10000); }
vector bool short testb_1e (vector bool short x) { return vec_splat (x, 0b11110); }
vector bool short testb_1f (vector bool short x) { return vec_splat (x, 0b11111); }

vector signed short tests_00 (vector signed short x) { return vec_splat (x, 0b00000); }
vector signed short tests_01 (vector signed short x) { return vec_splat (x, 0b00001); }
vector signed short tests_02 (vector signed short x) { return vec_splat (x, 0b00010); }
vector signed short tests_04 (vector signed short x) { return vec_splat (x, 0b00100); }
vector signed short tests_08 (vector signed short x) { return vec_splat (x, 0b01000); }
vector signed short tests_10 (vector signed short x) { return vec_splat (x, 0b10000); }
vector signed short tests_1e (vector signed short x) { return vec_splat (x, 0b11110); }
vector signed short tests_1f (vector signed short x) { return vec_splat (x, 0b11111); }

vector unsigned short testu_00 (vector unsigned short x) { return vec_splat (x, 0b00000); }
vector unsigned short testu_01 (vector unsigned short x) { return vec_splat (x, 0b00001); }
vector unsigned short testu_02 (vector unsigned short x) { return vec_splat (x, 0b00010); }
vector unsigned short testu_04 (vector unsigned short x) { return vec_splat (x, 0b00100); }
vector unsigned short testu_08 (vector unsigned short x) { return vec_splat (x, 0b01000); }
vector unsigned short testu_10 (vector unsigned short x) { return vec_splat (x, 0b10000); }
vector unsigned short testu_1e (vector unsigned short x) { return vec_splat (x, 0b11110); }
vector unsigned short testu_1f (vector unsigned short x) { return vec_splat (x, 0b11111); }

/* Similar test as above, but the source vector is a known constant. */
vector bool short test_bs () { const vector bool short y = {1, 2, 3, 4, 5, 6, 7, 8}; return vec_splat (y, 0b00010); }
vector signed short test_ss () { const vector signed short y = {1, 2, 3, 4, 5, 6, 7, 8}; return vec_splat (y, 0b00010); }
vector unsigned short test_us () { const vector unsigned short y = {1, 2, 3, 4, 5, 6, 7, 8}; return vec_splat (y, 0b00010); }

/* Similar tests as above, mask is greater than number of elements in the
 source vector.  */
vector bool short test_obs () { const vector bool short y = {1, 2, 3, 4, 5, 6, 7, 8}; return vec_splat (y, 0b10010); }
vector signed short test_oss () { const vector signed short y = {1, 2, 3, 4, 5, 6, 7, 8}; return vec_splat (y, 0b10010); }
vector unsigned short test_ous () { const vector unsigned short y = {1, 2, 3, 4, 5, 6, 7, 8}; return vec_splat (y, 0b10010); }

/* { dg-final { scan-assembler-times "vspltish" 6 } } */
/* { dg-final { scan-assembler-times "vsplth" 24 } } */
