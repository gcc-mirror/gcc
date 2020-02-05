// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
export module stuart;
// { dg-module-cmi !stuart } 

# 6 "atom-preamble-2_f.C" 1
import kevin; // { dg-error "not be from header" }
# 8 "" 2

import kevin; // ok
// { dg-prune-output "not writing module" }
