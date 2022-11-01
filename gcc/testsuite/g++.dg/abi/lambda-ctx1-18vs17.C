// { dg-do compile { target c++20 } }
// { dg-options {-fabi-version=18 -Wabi=17} }

#include "lambda-ctx1.h"

// { dg-regexp {[^\n]*lambda-ctx1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK1B2L3MUlT_T0_E_clIjdEEvS0_S1_'\) and '-fabi-version=18' \('_ZNK1B2L3MUlTyTyT_T0_E_clIjdEEvS1_S2_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-ctx1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK2L2MUlT_T0_E_clIifEEvS_S0_'\) and '-fabi-version=18' \('_ZNK2L2MUlTyTyT_T0_E_clIifEEvS0_S1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-ctx1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZNK1C1fMUlT_E_clIMS_iEEDaS0_'\) and '-fabi-version=18' \('_ZNK1C1fMUlT_E_clIMS_iEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-ctx1.h:[:0-9]* warning: the mangled name [^\n]* \('_Z3fooIN1qMUlvE_EN1qMUlvE0_EEiOT_OT0_'\) and '-fabi-version=18' \('_Z3fooIN1qMUlvE_ENS0_UlvE0_EEiOT_OT0_'\) [^\n]*\n} }
