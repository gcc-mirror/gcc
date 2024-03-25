// Test that ARM NEON types used by the Cryptograpy Extensions
// have their names mangled correctly.

// { dg-do compile }
// { dg-require-effective-target arm_crypto_ok }
// { dg-add-options arm_crypto }
// { dg-additional-options -fabi-compat-version=0 }

#include <arm_neon.h>

void f0 (poly64_t a) {}
void f1 (poly128_t a) {}
void f2 (poly64x2_t a) {}

// { dg-final { scan-assembler "_Z2f0y:" } }
// { dg-final { scan-assembler "_Z2f1o:" } }
// { dg-final { scan-assembler "_Z2f2Dv2_y:" } }
