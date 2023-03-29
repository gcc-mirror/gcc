// { dg-do compile { target c++14 } }
// { dg-additional-options -fabi-version=17 }

// PRs 78621

#include "lambda-mangle-1.h"

// We erroneously mangled lambda auto parms as-if template parameters (T<n>_),
// rather than auto (Da). Fixed in abi version 11
// Up to abi version 17 we had a single per-scope discriminator

// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRT_E_EOS0_S1_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRiRT_E0_EOS1_S2_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRT_R1XIiEE1_EOS0_S1_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlPA5_T_E2_EOS0_RS0_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRT_E_EvS1_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRiRT_E0_EvS2_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRT_R1XIiEE1_EvS1_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlPA5_T_E2_EvRS0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3FoovEUlPfS1_E3_EvRT_RT0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3FoovEUlPT_PT0_E4_EvRS1_RS3_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlPT_PT0_E4_Z3FoovEUlS1_S3_E5_EvRS0_RS2_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsPfS3_E_EvRT_RT0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsPT_PT0_E0_EvRS3_RS5_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsDpPT_E1_EvRT_RT0_:" } }
