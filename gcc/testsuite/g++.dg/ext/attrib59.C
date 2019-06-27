// PR c++/89325
// { dg-do compile }
// { dg-options "-Wattributes" }

int foo (int) { return 0; }	// { dg-bogus "previous definition" }
int bar (int) { return 0; }	// { dg-bogus "previous definition" }
int baz (int) { return 0; }	// { dg-message "previous definition" }
__attribute__((optimize (0))) int bar (long); // { dg-bogus "optimization attribute on '\[^\n\r]*' follows definition but the attribute doesn.t match" }
#pragma GCC optimize ("-fno-ipa-cp-clone")
int foo (long);			// { dg-bogus "optimization attribute on '\[^\n\r]*' follows definition but the attribute doesn.t match" }
int baz (int);			// { dg-warning "optimization attribute on '\[^\n\r]*' follows definition but the attribute doesn.t match" }
