/* Verify that overloaded built-ins for vec_splat with long long
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector bool long long testb_00 (vector bool long long x) { return vec_splat (x, 0b00000); }
vector bool long long testb_01 (vector bool long long x) { return vec_splat (x, 0b00001); }
vector bool long long testb_02 (vector bool long long x) { return vec_splat (x, 0b00010); }
vector bool long long testb_04 (vector bool long long x) { return vec_splat (x, 0b00100); }
vector bool long long testb_08 (vector bool long long x) { return vec_splat (x, 0b01000); }
vector bool long long testb_10 (vector bool long long x) { return vec_splat (x, 0b10000); }
vector bool long long testb_1e (vector bool long long x) { return vec_splat (x, 0b11110); }
vector bool long long testb_1f (vector bool long long x) { return vec_splat (x, 0b11111); }

vector signed long long tests_00 (vector signed long long x) { return vec_splat (x, 0b00000); }
vector signed long long tests_01 (vector signed long long x) { return vec_splat (x, 0b00001); }
vector signed long long tests_02 (vector signed long long x) { return vec_splat (x, 0b00010); }
vector signed long long tests_04 (vector signed long long x) { return vec_splat (x, 0b00100); }
vector signed long long tests_08 (vector signed long long x) { return vec_splat (x, 0b01000); }
vector signed long long tests_10 (vector signed long long x) { return vec_splat (x, 0b10000); }
vector signed long long tests_1e (vector signed long long x) { return vec_splat (x, 0b11110); }
vector signed long long tests_1f (vector signed long long x) { return vec_splat (x, 0b11111); }

vector unsigned long long testu_00 (vector unsigned long long x) { return vec_splat (x, 0b00000); }
vector unsigned long long testu_01 (vector unsigned long long x) { return vec_splat (x, 0b00001); }
vector unsigned long long testu_02 (vector unsigned long long x) { return vec_splat (x, 0b00010); }
vector unsigned long long testu_04 (vector unsigned long long x) { return vec_splat (x, 0b00100); }
vector unsigned long long testu_08 (vector unsigned long long x) { return vec_splat (x, 0b01000); }
vector unsigned long long testu_10 (vector unsigned long long x) { return vec_splat (x, 0b10000); }
vector unsigned long long testu_1e (vector unsigned long long x) { return vec_splat (x, 0b11110); }
vector unsigned long long testu_1f (vector unsigned long long x) { return vec_splat (x, 0b11111); }

/* Similar test as above, but the source vector is a known constant. */
vector bool long long test_bll () { const vector bool long long y = {12, 23}; return vec_splat (y, 0b00010); }
vector signed long long test_sll () { const vector signed long long y = {34, 45}; return vec_splat (y, 0b00010); }
vector unsigned long long test_ull () { const vector unsigned long long y = {56, 67}; return vec_splat (y, 0b00010); }

/* Similar tests as above, mask is greater than number of elements in the
 source vector.  */
vector bool long long test_obll () { const vector bool long long y = {12, 23}; return vec_splat (y, 0b10010); }
vector signed long long test_osll () { const vector signed long long y = {34, 45}; return vec_splat (y, 0b10010); }
vector unsigned long long test_oull () { const vector unsigned long long y = {56, 67}; return vec_splat (y, 0b10010); }

/* lvx for the initialization with known constants. */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M} 6 } } */

/* xxpermdi for vec_splat of long long vectors.
   At the time of this writing, the number of xxpermdi instructions
   generated could be 24 or 26 or 27, ultimately depending on the
   platform and whether or not folding is enabled.
   Roughly:
	24 occurrences on older targets (power5) regardless of folding state.
	26 occurrences with gimple folding enabled (through power9).
	27 occurrences with gimple folding disabled (through power9).
  So, ensure we have at least one hit.  */
/* { dg-final { scan-assembler "xxpermdi" } } */
