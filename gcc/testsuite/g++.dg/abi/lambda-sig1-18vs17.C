// { dg-do compile { target c++20 } }
// { dg-options {-fabi-version=18 -Wabi=17} }

#include "lambda-sig1.h"

// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfT_E7_clIiEEDafS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlTyfT_E1_clIiEEDafS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlT_E5_clIiEEDaS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlTyT_E1_clIiEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfE6_cvPFvfEEv'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlfE1_cvPFvfEEv'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENUlfE6_4_FUNEf'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENUlfE1_4_FUNEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfE6_clEf'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlfE1_clEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfT_E4_clIiEEDafS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlTyfT_E0_clIiEEDafS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlT_E2_clIiEEDaS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlTyT_E0_clIiEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfE3_cvPFvfEEv'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlfE0_cvPFvfEEv'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENUlfE3_4_FUNEf'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENUlfE0_4_FUNEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfE3_clEf'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlfE0_clEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfT_E1_clIiEEDafS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlTyfT_E_clIiEEDafS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlT_E_clIiEEDaS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlTyT_E_clIiEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfE0_cvPFvfEEv'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlfE_cvPFvfEEv'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENUlfE0_4_FUNEf'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENUlfE_4_FUNEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj1EE2FnEvENKUlfE0_clEf'\) and '-fabi-version=18' \('_ZZN1XIfLj1EE2FnEvENKUlfE_clEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENKUlfT_E4_clIiEEDafS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENKUlTyfT_E0_clIiEEDafS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENKUlT_E2_clIiEEDaS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENKUlTyT_E0_clIiEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENKUlfE3_cvPFvfEEv'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENKUlfE0_cvPFvfEEv'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENUlfE3_4_FUNEf'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENUlfE0_4_FUNEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENKUlfE3_clEf'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENKUlfE0_clEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENKUlfT_E1_clIiEEDafS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENKUlTyfT_E_clIiEEDafS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENKUlT_E_clIiEEDaS1_'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENKUlTyT_E_clIiEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENKUlfE0_cvPFvfEEv'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENKUlfE_cvPFvfEEv'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENUlfE0_4_FUNEf'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENUlfE_4_FUNEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIfLj0EE2FnEvENKUlfE0_clEf'\) and '-fabi-version=18' \('_ZZN1XIfLj0EE2FnEvENKUlfE_clEf'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENKUliT_E4_clIiEEDaiS1_'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENKUlTyiT_E0_clIiEEDaiS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENKUlT_E2_clIiEEDaS1_'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENKUlTyT_E0_clIiEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENKUliE3_cvPFviEEv'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENKUliE0_cvPFviEEv'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENUliE3_4_FUNEi'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENUliE0_4_FUNEi'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENKUliE3_clEi'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENKUliE0_clEi'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENKUliT_E1_clIiEEDaiS1_'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENKUlTyiT_E_clIiEEDaiS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENKUlT_E_clIiEEDaS1_'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENKUlTyT_E_clIiEEDaS1_'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENKUliE0_cvPFviEEv'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENKUliE_cvPFviEEv'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENUliE0_4_FUNEi'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENUliE_4_FUNEi'\) [^\n]*\n} }
// { dg-regexp {[^\n]*lambda-sig1.h:[:0-9]* warning: the mangled name [^\n]* \('_ZZN1XIiLj0EE2FnEvENKUliE0_clEi'\) and '-fabi-version=18' \('_ZZN1XIiLj0EE2FnEvENKUliE_clEi'\) [^\n]*\n} }
