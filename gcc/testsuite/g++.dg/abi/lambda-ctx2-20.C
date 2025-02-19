// { dg-do compile { target c++17 } }
// { dg-options -fabi-version=20 }

#include "lambda-ctx2.h"

// These correctly associate lambdas in A::x and B::x.
// { dg-final { scan-assembler {_ZNK1A1xMUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1BIiE1xMUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1BIiE1xMUlvE0_clEv:} } }
// { dg-final { scan-assembler {_ZNK1CIiE1xMUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1D1xIiEUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1EIiE1xIiEUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1EIiE1xIiEUlvE0_clEv:} } }
