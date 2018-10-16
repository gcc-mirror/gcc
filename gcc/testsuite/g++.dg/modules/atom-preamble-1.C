// { dg-additional-options -fmodules-atom }
// This test is messed up unti I complete the tokenizer transition

#define EXPORT export
EXPORT module bob; // { dg-error "expected" "" { xfail *-*-* } }
// { dg-prune-output "compilation terminated" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
