// { dg-do compile { target c++11 } }
#include <cstddef>
using namespace std;

// Check that we do not look for poisoned identifier when it is a suffix.
int _ħ;
#pragma GCC poison _ħ
const char * operator ""_ħ (const char *, size_t); // { dg-bogus "poisoned" }
bool operator ""_ħ (unsigned long long x); // { dg-bogus "poisoned" }
bool b = 1_ħ; // { dg-bogus "poisoned" }
const char *x = "hbar"_ħ; // { dg-bogus "poisoned" }

/* Ideally, we should not warn here either, but this is not implemented yet.  This
   syntax has been deprecated for C++23.  */
#pragma GCC poison _ħ2
const char * operator "" _ħ2 (const char *, size_t); // { dg-bogus "poisoned" "" { xfail *-*-*} }
// { dg-warning "space" "" { target c++23 } .-1 }
const char *x2 = "hbar2"_ħ2; // { dg-bogus "poisoned" }
