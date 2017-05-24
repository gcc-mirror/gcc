// PR c++/69317 - [6 regression] wrong ABI version in -Wabi warnings 
// Exercise that the correct ABI versions are referenced in the -Wabi
// diagnostic.  See also the equivalent Wabi-3-2.C test.

// { dg-options "-Wabi=2 -fabi-version=3" }
// { dg-do compile }

// The mangling of templates with a non-type template parameter
// of reference type changed in ABI version 3: 
extern int N;
template <int &> struct S { };

// Expect the diagnostic to reference the ABI version specified via
// -fabi-version=3 and the ABI version specified via -Wabi=2.
void foo (S<N>) { }   // { dg-warning "the mangled name of .void foo\\(S<N>\\). changed between .-fabi-version=2. \\(._Z3foo1SILZ1NEE.\\) and .-fabi-version=3. \\(._Z3foo1SIL_Z1NEE.\\)" }

