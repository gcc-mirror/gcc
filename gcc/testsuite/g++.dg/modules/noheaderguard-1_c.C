// { dg-do preprocess }
// { dg-additional-options {-fmodules-ts -fno-header-guard-opt} }

import "noheaderguard-1_a.H";
#include "noheaderguard-1_a.H"

// { dg-final { scan-file noheaderguard-1_c.i {# [0-9]* "[^\n]*noheaderguard-1_c.C"\n\n*import  "[^\n]*noheaderguard-1_a.H";\nimport  "[^\n]*noheaderguard-1_a.H" \[\[__translated\]\];\n} } }
