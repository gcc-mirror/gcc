// PR target/103973
// { dg-do run { target large_long_double } }
// { dg-options "-O2 -ffast-math -std=c++20 -save-temps" }
// { dg-final { scan-assembler-not "'\tfucom" } }
// { dg-final { scan-assembler-times "\tfcom" 2 } }

#define double_type long double
#include "pr103973-13.C"
