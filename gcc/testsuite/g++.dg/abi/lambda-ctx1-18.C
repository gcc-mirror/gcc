// { dg-do compile { target c++20 } }
// { dg-options -fabi-version=18 }

#include "lambda-ctx1.h"

// These correctly include the lambda's extra context as a
// substitution candidate, and thus demangle as expected
// { dg-final { scan-assembler {_ZNK1C1fMUlT_E_clIMS_iEEDaS1_:} } }
// { dg-final { scan-assembler {_ZNK2L2MUlTyTyT_T0_E_clIifEEvS0_S1_:} } }
// { dg-final { scan-assembler {_ZNK1B2L3MUlTyTyT_T0_E_clIjdEEvS1_S2_:} } }
// { dg-final { scan-assembler {_Z3fooIN1qMUlvE_ENS0_UlvE0_EEiOT_OT0_:} } }
