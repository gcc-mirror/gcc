// PR target/103973
// { dg-do compile { target ia32 } }
// { dg-options "-O2 -ffast-math -march=i686 -mfpmath=387 -std=c++20" }
// { dg-final { scan-assembler-not "'\tfucom" } }
// { dg-final { scan-assembler-times "\tfcom" 2 } }

#define double_type float
#include "pr103973-13.C"
