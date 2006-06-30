// Test for explicit visibility on template vs. #pragma vis at explicit
// instantiation/specialization point for plain function templates.

// { dg-require-visibility "" }
// { dg-final { scan-hidden "_Z3fooIdEvT_" } }
// { dg-final { scan-hidden "_Z3fooIlEvT_" } }
// { dg-final { scan-hidden "_Z3fooIcEvT_" } }
// { dg-final { scan-hidden "_Z3fooIiEvT_" } }
// { dg-final { scan-not-hidden "_Z3fooIfEvT_" } }
// { dg-final { scan-not-hidden "_Z3fooIsEvT_" } }

// { dg-final { scan-hidden "_Z3barIdEvT_" } }
// { dg-final { scan-hidden "_Z3barIlEvT_" } }
// { dg-final { scan-hidden "_Z3barIiEvT_" } }
// { dg-final { scan-hidden "_Z3barIcEvT_" } }
// { dg-final { scan-not-hidden "_Z3barIfEvT_" } }
// { dg-final { scan-not-hidden "_Z3barIsEvT_" } }

#pragma GCC visibility push(hidden)
template <class T> void bar(T) { }
#pragma GCC visibility pop
template void bar (long);
template<> void bar (double) { }
template __attribute ((visibility ("default"))) void bar (short);
template<> __attribute ((visibility ("default"))) void bar (float) { }
#pragma GCC visibility push(default)
template<> void bar(char) { }
template void bar(int);
#pragma GCC visibility pop

template <class T> __attribute ((visibility ("hidden"))) void foo(T) { }
template void foo (long);
template<> void foo (double) { }
template __attribute ((visibility ("default"))) void foo (short);
template<> __attribute ((visibility ("default"))) void foo (float) { }
#pragma GCC visibility push(default)
template<> void foo(char) { }
template void foo(int);
#pragma GCC visibility pop
