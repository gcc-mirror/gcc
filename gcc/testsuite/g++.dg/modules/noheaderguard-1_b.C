// { dg-do preprocess }
// { dg-additional-options {-fmodules-ts -fno-header-guard-opt} }

#include "noheaderguard-1_a.H"
#include "noheaderguard-1_a.H"

// { dg-final { scan-file noheaderguard-1_b.i {# [0-9]* "[^\n]*noheaderguard-1_b.C"\n\n*import  "[^\n]*noheaderguard-1_a.H" \[\[__translated\]\];\nimport  "[^\n]*noheaderguard-1_a.H" \[\[__translated\]\];\n} } }
