// { dg-do compile { target c++20 } }
// { dg-options -fabi-version=17 }

#include "lambda-sig1.h"

// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlfE0_clEf:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlfE3_clEf:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlfT_E1_clIiEEDafS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlfT_E4_clIiEEDafS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlT_E2_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlT_E_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfE0_clEf:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfE3_clEf:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfE6_clEf:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfT_E1_clIiEEDafS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfT_E4_clIiEEDafS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfT_E7_clIiEEDafS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlT_E2_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlT_E5_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlT_E_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUliE0_clEi:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUliE3_clEi:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUliT_E1_clIiEEDaiS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUliT_E4_clIiEEDaiS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUlT_E2_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUlT_E_clIiEEDaS1_:} } }
