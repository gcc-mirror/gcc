// { dg-do compile { target c++17 } }
// { dg-options -fabi-version=19 }

#include "lambda-ctx2.h"

// A and B demangle incorrectly due to static data members missing a lambda scope
// { dg-final { scan-assembler {_ZNK1AUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1BIiEUlvE2_clEv:} } }
// { dg-final { scan-assembler {_ZNK1BIiEUlvE3_clEv:} } }
// { dg-final { scan-assembler {_ZNK1CIiE1xMUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1DUlvE7_clEv:} } }
// { dg-final { scan-assembler {_ZNK1EIiEUlvE8_clEv:} } }
// { dg-final { scan-assembler {_ZNK1EIiEUlvE9_clEv:} } }
