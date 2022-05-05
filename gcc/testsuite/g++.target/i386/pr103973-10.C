// PR target/103973
// { dg-do compile { target ia32 } }
// { dg-options "-O2 -march=i686 -mfpmath=387 -std=c++20" }
// { dg-final { scan-assembler-not "'\tfucom" } }
// { dg-final { scan-assembler-times "\tfcom" 2 } }

#include "pr103973-9.C"
