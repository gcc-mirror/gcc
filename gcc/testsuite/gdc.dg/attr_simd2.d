// { dg-do compile }
// { dg-options "-fdump-tree-optimized" }

import gcc.attributes;

@attribute("simd")
int simd_ignored; // { dg-warning ".simd. attribute ignored" }

@attribute("simd", 123)
int simd_string() { return 0; } // { dg-error ".simd. attribute argument not a string constant" }

@attribute("simd", "invalid")
int simd_invalid() { return 0; } // { dg-error "only .inbranch. and .notinbranch. flags are allowed for .simd. attribute" }

@attribute("simd", "notinbranch", "inbranch")
int simd_wrong_args() { return 0; } // { dg-error "wrong number of arguments specified for .simd. attribute" }
