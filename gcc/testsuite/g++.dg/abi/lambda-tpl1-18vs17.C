// { dg-do compile { target c++20 } }
// { dg-options {-fabi-version=18 -Wabi=17} }

#include "lambda-tpl1.h"

// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZ2FnILi1EEvvENKUlT_E_clIiEEDaS0_'\) and '-fabi-version=18' \('_ZZ2FnILi1EEvvENKUlTyT_E_clIiEEDaS0_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK6l_var4MUlR1YIJDpT_EEE_clIJ1US6_EEEDaS3_'\) and '-fabi-version=18' \('_ZNK6l_var4MUlTpTtTyTnjER1YIJDpT_EEE_clIJ1US7_EEEDaS4_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK6l_var3MUlRT_IJXspT0_EEEE_clI1XJLi1ELi2ELi3EEEEDaS1_'\) and '-fabi-version=18' \('_ZNK6l_var3MUlTtTpTniETpTniRT_IJXspT0_EEEE_clI1XJLi1ELi2ELi3EEEEDaS2_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK5l_varMUlDpT_E_clIJiiiEEEDaS0_'\) and '-fabi-version=18' \('_ZNK5l_varMUlTpTyDpT_E_clIJiiiEEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK13l_tpl_tpl_tplMUlR6TPLTPLIT_EE_clI3TPLEEDaS2_'\) and '-fabi-version=18' \('_ZNK13l_tpl_tpl_tplMUlTtTtTyTnjEER6TPLTPLIT_EE_clI3TPLEEDaS3_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK9l_tpl_tplMUlR3TPLIT_EE_clI1UEEDaS2_'\) and '-fabi-version=18' \('_ZNK9l_tpl_tplMUlTtTyTnjER3TPLIT_EE_clI1UEEDaS3_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK13l_tpl_nt_autoMUlvE_clILi0EEEDav'\) and '-fabi-version=18' \('_ZNK13l_tpl_nt_autoMUlTnDavE_clILi0EEEDav'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK12l_tpl_nt_aryMUlRAT__iE_clILi2EEEDaS0_'\) and '-fabi-version=18' \('_ZNK12l_tpl_nt_aryMUlTniRAT__iE_clILi2EEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK10l_tpl_autoMUlT_T0_E_clIiiEEDaS_S0_'\) and '-fabi-version=18' \('_ZNK10l_tpl_autoMUlTyT_T0_E_clIiiEEDaS0_S1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK5l_tplMUlT_E_clIiEEDaS_'\) and '-fabi-version=18' \('_ZNK5l_tplMUlTyT_E_clIiEEDaS0_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK6l_autoMUlT_E_clIiEEDaS_'\) and '-fabi-version=18' \('_ZNK6l_autoMUlT_E_clIiEEDaS0_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZ1fvENKUlT_E_clIiLi0EEEDaS_'\) and '-fabi-version=18' \('_ZZ1fvENKUlTyTnT_S_E_clIiLi0EEEDaS_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZZ1fvENKUlT_E_clIiLi0EEEDaS_ENKUliS_E_clIiEEDaiS_'\) and '-fabi-version=18' \('_ZZZ1fvENKUlTyTnT_S_E_clIiLi0EEEDaS_ENKUlTyiS_E_clIiEEDaiS_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZ1fvENKUlT_E_clIcLc0EEEDaS_'\) and '-fabi-version=18' \('_ZZ1fvENKUlTyTnT_S_E_clIcLc0EEEDaS_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZZ1fvENKUlT_E_clIcLc0EEEDaS_ENKUlcS_E_clIcEEDacS_'\) and '-fabi-version=18' \('_ZZZ1fvENKUlTyTnT_S_E_clIcLc0EEEDaS_ENKUlTycS_E_clIcEEDacS_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-tpl1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZ1fvENKUlP1UIT_Lj0EEPS_IiLj0EEE0_clIcEEDaS2_S4_'\) and '-fabi-version=18' \('_ZZ1fvENKUlTyP1UIT_Lj0EEPS_IiLj0EEE_clIcEEDaS2_S4_'\) [^\n]*\n} }
