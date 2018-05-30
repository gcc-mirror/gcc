// { dg-options "-fmodules-ts" }

#ifndef __cpp_modules_ts
#error "Wat?"
#endif

#ifdef __cpp_modules_atom
#error "Wat?"
#endif


import bob;
// { dg-error "failed to read"  "bob:" { target *-*-* } 0 }
// { dg-error "TS/ATOM mismatch" "bob:" { target *-*-* } 0 }
// { dg-error "jumping off" "" { target *-*-* } 0 }
// { dg-prune-output "compilation terminated" }
