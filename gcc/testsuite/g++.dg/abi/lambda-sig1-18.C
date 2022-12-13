// { dg-do compile { target c++20 } }
// { dg-options -fabi-version=18 }

#include "lambda-sig1.h"

// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlfE_clEf:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlfE0_clEf:} } }

// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlTyfT_E_clIiEEDafS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlTyfT_E0_clIiEEDafS1_:} } }

// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlTyT_E_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj0EE2FnEvENKUlTyT_E0_clIiEEDaS1_:} } }

// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfE_clEf:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfE0_clEf:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlfE1_clEf:} } }

// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlTyfT_E_clIiEEDafS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlTyfT_E0_clIiEEDafS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlTyfT_E1_clIiEEDafS1_:} } }

// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlTyT_E_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlTyT_E0_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIfLj1EE2FnEvENKUlTyT_E1_clIiEEDaS1_:} } }

// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUliE_clEi:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUliE0_clEi:} } }

// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUlTyiT_E_clIiEEDaiS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUlTyiT_E0_clIiEEDaiS1_:} } }

// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUlTyT_E_clIiEEDaS1_:} } }
// { dg-final { scan-assembler {_ZZN1XIiLj0EE2FnEvENKUlTyT_E0_clIiEEDaS1_:} } }
