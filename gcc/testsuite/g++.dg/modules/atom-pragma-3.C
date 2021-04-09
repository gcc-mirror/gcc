// { dg-additional-options "-fmodules-ts -Wno-pedantic" }

export module foo;
// { dg-module-cmi !foo }
;

#pragma pack(2)
import baz; // { dg-error "must be contiguous" }

int i;

// { dg-prune-output "not writing module" }
