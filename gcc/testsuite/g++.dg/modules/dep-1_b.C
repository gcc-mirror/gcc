// { dg-additional-options "-fmodules-ts -MD" }
export module m;
// { dg-module-cmi m }

export import :part;
// { dg-final { scan-file dep-1_b.d {\ndep-1_b\.s gcm.cache/m\.gcm: m:part\.c\+\+-module} } }
// { dg-final { scan-file dep-1_b.d {\nm\.c\+\+-module: gcm.cache/m\.gcm} } }
// { dg-final { scan-file dep-1_b.d {\n\.PHONY: m\.c\+\+-module} } }
// { dg-final { scan-file dep-1_b.d {\ngcm.cache/m\.gcm:| dep-1_b.o} } }
// { dg-final { scan-file dep-1_b.d {\nCXX_IMPORTS \+= m:part\.c\+\+-module} } }
