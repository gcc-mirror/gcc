// { dg-do compile { target c++14 } }
// { dg-additional-options -fabi-version=18 }

// PRs 78621

#include "lambda-mangle-1.h"

// We erroneously mangled lambda auto parms as-if template parameters (T<n>_),
// rather than auto (Da). Fixed in abi version 11

// ABI 18 uses per-scope per-signature lambda discriminator

// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRT_E_EOS0_S1_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRiRT_E_EOS1_S2_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRT_R1XIiEE_EOS0_S1_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlPA5_T_E_EOS0_RS0_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRT_E_EvS1_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRiRT_E_EvS2_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRT_R1XIiEE_EvS1_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlPA5_T_E_EvRS0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3FoovEUlPfS1_E_EvRT_RT0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3FoovEUlPT_PT0_E_EvRS1_RS3_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlPT_PT0_E_Z3FoovEUlS1_S3_E0_EvRS0_RS2_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsPfS3_E_EvRT_RT0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsPT_PT0_E_EvRS3_RS5_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsDpPT_E_EvRT_RT0_:" } }
