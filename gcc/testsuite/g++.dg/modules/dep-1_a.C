// { dg-additional-options "-fmodules-ts -MD" }

export module m:part;
// { dg-module-cmi m:part }

// All The Backslashes!
// { dg-final { scan-file dep-1_a.d {\nm\\:part\.c\+\+m: m\\:part\.gcm} } }
// { dg-final { scan-file dep-1_a.d {\nm\\:part\.gcm:| dep-1_a\.o} } }
// { dg-final { scan-file dep-1_a.d {\n\.PHONY: m\\:part\.c\+\+m} } }
