// PR c++/50372
// Test that a template instantiation has the same linkage as its argument.
// { dg-final { scan-assembler "(weak|glob)\[^\n\]*_Z3fooIXadL_Z13external_funcvEEEvv" } }
// { dg-final { scan-assembler-not "(weak|glob)\[^\n\]*_Z3fooIXadL_ZL11static_funcvEEEvv" { xfail powerpc-*-aix* } } }

template<void (*fptr)(void)>
void foo() { }

static void static_func() {}
void external_func() { }

void test()
{
#if __cplusplus > 199711L
  foo<&static_func>();
#endif
  foo<&external_func>();
}
