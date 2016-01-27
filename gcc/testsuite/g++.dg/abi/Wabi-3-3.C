// Verify that no diagnostic is issued when the version specified
// via -Wabi= matches the version specified by -fabi-version=.

// { dg-options "-Werror -Wabi=3 -fabi-version=3" }
// { dg-do compile }

// The mangling of templates with a non-type template parameter
// of reference type changed in ABI version 3: 
extern int N;
template <int &> struct S { };

// Expect no diagnostic.
void foo (S<N>) { }

