/* Verify that overloaded built-ins for vec_splat with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool char testb_0  (vector bool char x) { return vec_splat (x, 0b00000); }
vector bool char testb_1  (vector bool char x) { return vec_splat (x, 0b00001); }
vector bool char testb_2  (vector bool char x) { return vec_splat (x, 0b00010); }
vector bool char testb_4  (vector bool char x) { return vec_splat (x, 0b00100); }
vector bool char testb_8  (vector bool char x) { return vec_splat (x, 0b01000); }
vector bool char testb_10 (vector bool char x) { return vec_splat (x, 0b10000); }
vector bool char testb_1e (vector bool char x) { return vec_splat (x, 0b11110); }
vector bool char testb_1f (vector bool char x) { return vec_splat (x, 0b11111); }

vector signed char tests_0  (vector signed char x) { return vec_splat (x, 0b00000); }
vector signed char tests_1  (vector signed char x) { return vec_splat (x, 0b00001); }
vector signed char tests_2  (vector signed char x) { return vec_splat (x, 0b00010); }
vector signed char tests_4  (vector signed char x) { return vec_splat (x, 0b00100); }
vector signed char tests_8  (vector signed char x) { return vec_splat (x, 0b01000); }
vector signed char tests_10 (vector signed char x) { return vec_splat (x, 0b10000); }
vector signed char tests_1e (vector signed char x) { return vec_splat (x, 0b11110); }
vector signed char tests_1f (vector signed char x) { return vec_splat (x, 0b11111); }

vector unsigned char testu_0  (vector unsigned char x) { return vec_splat (x, 0b00000); }
vector unsigned char testu_1  (vector unsigned char x) { return vec_splat (x, 0b00001); }
vector unsigned char testu_2  (vector unsigned char x) { return vec_splat (x, 0b00010); }
vector unsigned char testu_4  (vector unsigned char x) { return vec_splat (x, 0b00100); }
vector unsigned char testu_8  (vector unsigned char x) { return vec_splat (x, 0b01000); }
vector unsigned char testu_10 (vector unsigned char x) { return vec_splat (x, 0b10000); }
vector unsigned char testu_1e (vector unsigned char x) { return vec_splat (x, 0b11110); }
vector unsigned char testu_1f (vector unsigned char x) { return vec_splat (x, 0b11111); }

/* Similar tests as above, but the source vector is a known constant. */
const vector bool char by = {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p'};
const vector signed char sy = {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p'};
const vector unsigned char uy = {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p'};

vector bool char test_bc (vector bool char x) { return vec_splat (by, 0b00010); }
vector signed char test_sc (vector signed char x) { return vec_splat (sy, 0b00011); }
vector unsigned char test_uc (vector unsigned char x) { return vec_splat (uy, 0b00110); }

/* Similar tests as above, mask is greater than number of elements in the
 source vector.  */
vector bool char test_obc (vector bool char x) { return vec_splat (by, 0b10010); }
vector signed char test_osc (vector signed char x) { return vec_splat (sy, 0b10011); }
vector unsigned char test_ouc (vector unsigned char x) { return vec_splat (uy, 0b10110); }

// vec_splat() using variable vectors should generate the vspltb instruction.
/* { dg-final { scan-assembler-times "vspltb" 24 } } */
// vec_splat() using a constant vector should generate a load.
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvw4x\M} 6 } } */
