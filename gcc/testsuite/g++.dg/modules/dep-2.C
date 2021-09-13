// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -MD" }

module m:part;
// { dg-module-cmi !m:part }

// All The Backslashes!
// { dg-final { scan-file dep-2.d {\nm:part\.c\+\+m: gcm.cache/m-part\.gcm} } }
// { dg-final { scan-file dep-2.d {\ngcm.cache/m:part\.gcm:| dep-2\.o} } }
// { dg-final { scan-file dep-2.d {\n\.PHONY: m:part\.c\+\+m} } }

// { dg-final { scan-file dep-2.i {\nmodule  m:part;\n} } }
