// PR c++/19134
// -fvisibility-inlines-hidden doesn't apply to non-inline specializations

// { dg-require-visibility "" }
// { dg-options "-fvisibility-inlines-hidden" }
// { dg-final { scan-not-hidden "_ZN1AIiE3fooEv" } }
// { dg-final { scan-not-hidden "_ZN1AIiE3barEv" } }
// { dg-final { scan-hidden "_ZN1AIlE3fooEv" } }
// { dg-final { scan-hidden "_ZN1AIlE3barEv" } }
// { dg-final { scan-hidden "_ZN1AIcE3barEv" } }

template<class T>
struct A {
  void foo() {};
  __attribute ((visibility ("hidden"))) void bar();
};

// This has default visibility.
template<> void A<int>::foo() {}

// This has hidden visibility because of -fvisibility-inlines-hidden.
template<> inline void A<long>::foo() {}
// Force the inline out.
void f () { A<long> a; a.foo(); }

// This has default visibility.
template<> __attribute ((visibility ("default"))) void A<int>::bar() {}

// This inherits hidden visibility from its template.
template<> void A<long>::bar() { }

// This also has hidden visibility; #pragma vis doesn't affect class members.
#pragma GCC visibility push(default)
template<> void A<char>::bar() { }
#pragma GCC visibility pop
