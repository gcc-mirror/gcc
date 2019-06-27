// PR c++/89325
// { dg-do compile }
// { dg-options "-Wattributes" }

struct A { friend int &operator<< (int &i, const A &) { return i; } };	// { dg-bogus "previous definition" }
#pragma GCC optimize ("-fno-ipa-cp-clone")
struct B {};
int &operator<<(int &, const B &);	// { dg-bogus "optimization attribute on '\[^\n\r]*' follows definition but the attribute doesn.t match" }
