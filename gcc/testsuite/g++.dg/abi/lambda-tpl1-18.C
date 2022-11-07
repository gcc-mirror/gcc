// { dg-do compile { target c++20 } }

#include "lambda-tpl1.h"

// { dg-final { scan-assembler {_ZNK6l_autoMUlT_E_clIiEEDaS0_:} } }
// { dg-final { scan-assembler {_ZNK5l_tplMUlTyT_E_clIiEEDaS0_:} } }
// { dg-final { scan-assembler {_ZNK10l_tpl_autoMUlTyT_T0_E_clIiiEEDaS0_S1_:} } }
// { dg-final { scan-assembler {_ZNK12l_tpl_nt_aryMUlTniRAT__iE_clILi2EEEDaS1_:} } }
// { dg-final { scan-assembler {_ZNK13l_tpl_nt_autoMUlTnDavE_clILi0EEEDav:} } }
// { dg-final { scan-assembler {_ZNK9l_tpl_tplMUlTtTyTnjER3TPLIT_EE_clI1UEEDaS3_:} } }
// { dg-final { scan-assembler {_ZNK13l_tpl_tpl_tplMUlTtTtTyTnjEER6TPLTPLIT_EE_clI3TPLEEDaS3_:} } }
// { dg-final { scan-assembler {_ZNK5l_varMUlTpTyDpT_E_clIJiiiEEEDaS1_:} } }

// { FIXMEdg-final { scan-assembler {_ZNK6l_var2MUlTpTniDpRAT__iE_clIJLi2ELi2EEEEDaS2_:} } }

// { dg-final { scan-assembler {_ZNK6l_var3MUlTtTpTniETpTniRT_IJXspT0_EEEE_clI1XJLi1ELi2ELi3EEEEDaS2_:} } }

// { dg-final { scan-assembler {_ZNK6l_var4MUlTpTtTyTnjER1YIJDpT_EEE_clIJ1US7_EEEDaS4_:} } }
// This is a different mangling to clang, which gets
// _ZNK6l_var4MUlTpTtTyTnjER1YIJDpT_EEE_clIJ1US6_EEEDaS3_
// However, I think that is incorrect -- it doesn't demangle as
// expected (using the llvm demangler).
// https://github.com/llvm/llvm-project/issues/58631

// { dg-final { scan-assembler {_ZZ2FnILi1EEvvENKUlTyT_E_clIiEEDaS0_:} } }

// { dg-final { scan-assembler {_ZZ1fvENKUlTyTnT_S_E_clIcLc0EEEDaS_:} } }
// { dg-final { scan-assembler {_ZZ1fvENKUlTyTnT_S_E_clIiLi0EEEDaS_:} } }
// { dg-final { scan-assembler {_ZZZ1fvENKUlTyTnT_S_E_clIcLc0EEEDaS_ENKUlTycS_E_clIcEEDacS_:} } }
// { dg-final { scan-assembler {_ZZZ1fvENKUlTyTnT_S_E_clIiLi0EEEDaS_ENKUlTyiS_E_clIiEEDaiS_:} } }
// { dg-final { scan-assembler {_ZZ1fvENKUlTyP1UIT_Lj0EEPS_IiLj0EEE_clIcEEDaS2_S4_:} } }
