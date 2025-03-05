// PR c++/119102
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

int foo (int)
#if defined __x86_64__ || defined __aarch64__
__attribute__((simd ("notinbranch")))
#endif
;
