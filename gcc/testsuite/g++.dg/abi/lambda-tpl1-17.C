// { dg-do compile { target c++20 } }
// { dg-options -fabi-version=17 }

#include "lambda-tpl1.h"

// { dg-final { scan-assembler {_ZNK6l_autoMUlT_E_clIiEEDaS_:} } }
// { dg-final { scan-assembler {_ZNK5l_tplMUlT_E_clIiEEDaS_:} } }
// { dg-final { scan-assembler {_ZNK10l_tpl_autoMUlT_T0_E_clIiiEEDaS_S0_:} } }
// { dg-final { scan-assembler {_ZNK12l_tpl_nt_aryMUlRAT__iE_clILi2EEEDaS0_:} } }
// { dg-final { scan-assembler {_ZNK13l_tpl_nt_autoMUlvE_clILi0EEEDav:} } }
// { dg-final { scan-assembler {_ZNK9l_tpl_tplMUlR3TPLIT_EE_clI1UEEDaS2_:} } }
// { dg-final { scan-assembler {_ZNK13l_tpl_tpl_tplMUlR6TPLTPLIT_EE_clI3TPLEEDaS2_:} } }
// { dg-final { scan-assembler {_ZNK5l_varMUlDpT_E_clIJiiiEEEDaS0_:} } }

// This mangling might not be correct, it is my best guess:
// { FIXMEdg-final { scan-assembler {_ZNK6l_var2MUlDpRAT__iE_clIJLi2ELi2EEEEDaS1_:} } }

// { dg-final { scan-assembler {_ZNK6l_var3MUlRT_IJXspT0_EEEE_clI1XJLi1ELi2ELi3EEEEDaS1_:} } }
// { dg-final { scan-assembler {_ZNK6l_var4MUlR1YIJDpT_EEE_clIJ1US6_EEEDaS3_:} } }
// { dg-final { scan-assembler {_ZZ2FnILi1EEvvENKUlT_E_clIiEEDaS0_:} } }

// { dg-final { scan-assembler {_ZZ1fvENKUlT_E_clIcLc0EEEDaS_:} } }
// { dg-final { scan-assembler {_ZZ1fvENKUlT_E_clIiLi0EEEDaS_:} } }
// { dg-final { scan-assembler {_ZZZ1fvENKUlT_E_clIcLc0EEEDaS_ENKUlcS_E_clIcEEDacS_:} } }
// { dg-final { scan-assembler {_ZZZ1fvENKUlT_E_clIiLi0EEEDaS_ENKUliS_E_clIiEEDaiS_:} } }
// { dg-final { scan-assembler {_ZZ1fvENKUlP1UIT_Lj0EEPS_IiLj0EEE0_clIcEEDaS2_S4_:} } }
