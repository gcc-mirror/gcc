// { dg-do compile { target c++20 } }
// { dg-options -fabi-version=17 }

#include "lambda-ctx1.h"

// These demangle incorrectly, due to a missed substitution candidate
// { dg-final { scan-assembler {_ZNK1C1fMUlT_E_clIMS_iEEDaS0_:} } }
// { dg-final { scan-assembler {_ZNK2L2MUlT_T0_E_clIifEEvS_S0_:} } }
// { dg-final { scan-assembler {_ZNK1B2L3MUlT_T0_E_clIjdEEvS0_S1_:} } }
// { dg-final { scan-assembler {_Z3fooIN1qMUlvE_EN1qMUlvE0_EEiOT_OT0_:} } }
