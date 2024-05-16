// { dg-additional-options "-fmodules-ts -MD" }

export module m:part;
// { dg-module-cmi m:part }

// All The Backslashes!
// { dg-final { scan-file dep-1_a.d {\nm:part\.c\+\+-module: gcm.cache/m-part\.gcm} } }
// { dg-final { scan-file dep-1_a.d {\ngcm.cache/m-part\.gcm:| dep-1_a\.o} } }
// { dg-final { scan-file dep-1_a.d {\n\.PHONY: m:part\.c\+\+-module} } }
