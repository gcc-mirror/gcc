// PR c++/118277
// { dg-do compile { target c++11 } }
// { dg-options "" }
// Override any default-'-fno-exceptions':
// { dg-additional-options -fexceptions }

struct A {};
struct B { int size; };
struct C { constexpr int size () const { return 0; } };
struct D { constexpr int size () const { return 0; } int data; };
struct E { int size = 0;
	   constexpr const char *data () const { return ""; } };
struct F { constexpr const char *size () const { return ""; }
	   constexpr const char *data () const { return ""; } };
struct G { constexpr long size () const { return 0; }
	   constexpr float data () const { return 0.0f; } };
struct H { short size () const { return 0; }
	   constexpr const char *data () const { return ""; } };
struct I { constexpr signed char size () const { return 0; }
	   const char *data () const { return ""; } };
struct J { constexpr int size () const { return j ? throw 1 : 0; }	// { dg-error "expression '<throw-expression>' is not a constant expression" }
	   constexpr const char *data () const { return ""; };
	   constexpr J (int x) : j (x) {}
	   int j; };
struct K { constexpr operator int () { return 4; } };
struct L { constexpr operator const char * () { return "test"; } };
struct M { constexpr K size () const { return {}; }
	   constexpr L data () const { return {}; } };
#if  __cpp_constexpr_dynamic_alloc >= 201907L
struct N { constexpr int size () const { return 3; }
	   constexpr const char *data () const { return new char[3] { 'b', 'a', 'd' }; } }; // { dg-error "'\\\* N\\\(\\\).N::data\\\(\\\)' is not a constant expression because allocated storage has not been deallocated" "" { target c++20 } }
#endif
constexpr const char a[] = { 't', 'e', 's', 't' };
struct O { constexpr int size () const { return 4; }
	   constexpr const char *data () const { return a; } };
struct P { constexpr int size () const { return 4 - p; }
	   constexpr const char *data () const { return &a[p]; }
	   constexpr P (int x) : p (x) {}
	   int p; };
struct Q { constexpr int size () const { return 4 - q; }
	   constexpr const char *data () const { return &"test"[q]; }
	   constexpr Q (int x) : q (x) {}
	   int q; };
struct R { constexpr int size () const { return 4 - r; }
	   constexpr const char *d () const { return "test"; }
	   constexpr const char *data () const { return d () + r; }
	   constexpr R (int x) : r (x) {}
	   int r; };
struct S { constexpr float size (float) const { return 42.0f; }
	   constexpr int size (void * = nullptr) const { return 4; }
	   constexpr double data (double) const { return 42.0; }
	   constexpr const char *data (int = 0) const { return "test"; } };
using size_t = decltype (sizeof (0));
struct string_view {
  size_t s;
  const char *d;
  constexpr string_view () : s (0), d (nullptr) {}
  constexpr string_view (const char *p) : s (__builtin_strlen (p)), d (p) {}
  constexpr string_view (size_t l, const char *p) : s (l), d (p) {}
  constexpr size_t size () const noexcept { return s; }
  constexpr const char *data () const noexcept { return d; }
};
template <typename T, size_t N>
struct array {
  constexpr size_t size () const { return N; }
  constexpr const T *data () const { return a; }
  const T a[N];
};
struct U { constexpr operator const char * () const { return u; }
	   char u[5] = "test"; };
#if __cplusplus >= 201402L
struct V { constexpr auto size () const { return K {}; }
	   constexpr auto data () const { return U {}; } };
#endif
struct W { constexpr int size (int) const { return 4; }
	   constexpr const char *data () const { return "test"; } };
struct X { constexpr int size () const { return 4; }
	   constexpr const char *data (int) const { return "test"; } };
struct Y { constexpr int size () { return 4; }
	   constexpr const char *data (int) { return "test"; } };
#if __cpp_concepts >= 201907L
struct Z { constexpr int size (auto...) const { return 4; }
	   constexpr const char *data (auto...) const { return "test"; } };
#endif

void
foo ()
{
  int v1, v2;
  asm ("");
  asm ((""));			// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "request for member 'size' in '\\\(\\\"\\\"\\\)', which is of non-class type 'const char \\\[1\\\]'" "" { target *-*-* } .-1 }
  asm (("" + 0));		// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "request for member 'size' in '\\\(const char\\\*\\\)\\\"\\\"', which is of non-class type 'const char\\\*'" "" { target *-*-* } .-1 }
  asm ((0) ::);			// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "request for member 'size' in '0', which is of non-class type 'int'" "" { target *-*-* } .-1 }
  asm ("" : (A {}) (v1));	// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "'struct A' has no member named 'size'" "" { target *-*-* } .-1 }
  asm ("" : : (B {}) (1));	// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "'struct B' has no member named 'data'" "" { target *-*-* } .-1 }
  asm ("" ::: (C {}));		// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "'struct C' has no member named 'data'" "" { target *-*-* } .-1 }
  asm ((D {}));			// { dg-error "'D\\\(\\\).D::data' cannot be used as a function" }
  asm ("" : (E {}) (v1));	// { dg-error "'E\\\{0\\\}.E::size' cannot be used as a function" }
  __asm ("" :: (F {}) (1));	// { dg-error "constexpr string 'size\\\(\\\)' must be implicitly convertible to 'std::size_t'" }
				// { dg-error "could not convert 'F\\\(\\\).F::size\\\(\\\)' from 'const char\\\*' to '\[^']*'" "" { target *-*-* } .-1 }
				// { dg-error "conversion from 'const char\\\*' to '\[^']*' in a converted constant expression" "" { target *-*-* } .-2 }
  asm ("" : : : (G {}));	// { dg-error "constexpr string 'data\\\(\\\)' must be implicitly convertible to 'const char\\\*'" }
				// { dg-error "could not convert 'G\\\(\\\).G::data\\\(\\\)' from 'float' to 'const char\\\*'" "" { target *-*-* } .-1 }
  asm ("" : "=r" (v1), (H {}) (v2)); // { dg-error "call to non-'constexpr' function 'short int H::size\\\(\\\) const'" }
				// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" "" { target *-*-* } .-1 }
  asm ((I {}));			// { dg-error "call to non-'constexpr' function 'const char\\\* I::data\\\(\\\) const'" }
				// { dg-error "constexpr string 'data\\\(\\\)' must be a core constant expression" "" { target *-*-* } .-1 }

  asm ((J (0)));
  asm ("" :: (J (1)) (1));	// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" }
  asm ((M {}));
#if __cpp_constexpr_dynamic_alloc >= 201907L
  asm ((N {}));			// { dg-error "constexpr string 'data\\\(\\\)\\\[0\\\]' must be a constant expression" "" { target c++20 } }
#endif
  asm ((O {}));
  asm ((P (0)));
  asm ((P (2)));
  asm ((Q (0)));
  asm ((Q (1)));
  asm ((R (0)));
  asm ((R (2)));
  asm ((S {}));

  asm ((string_view {}));
  asm ((string_view ("test")));
  asm ((string_view ("א")));
  asm ((string_view (0, nullptr)));
  asm ((string_view (4, "testwithextrachars")));
  asm ((string_view (42, "test")));				// { dg-error "array subscript value '41' is outside the bounds of array type 'const char \\\[5\\\]'" }
								// { dg-error "constexpr string 'data\\\(\\\)\\\[41\\\]' must be a constant expression" "" { target *-*-* } .-1 }

  asm ((array<char, 2> { 'O', 'K' }));
  asm ((array<wchar_t, 2> { L'O', L'K' }));			// { dg-error "constexpr string 'data\\\(\\\)' must be implicitly convertible to 'const char\\\*'" }
								// { dg-error "could not convert 'array<wchar_t, 2>{const wchar_t \\\[2\\\]{\[0-9]+, \[0-9]+}}.array<wchar_t, 2>::data\\\(\\\)' from 'const wchar_t\\\*' to 'const char\\\*'" "" { target *-*-* } .-1 }
  asm ((array<char, 4> { 't', 'e', 's', 't' }));
  {
    constexpr auto a = array<char, 4> { 't', 'e', 's', 't' };
    asm ((a));
  }

#if __cplusplus >= 201402L
  asm ((V {}));
#endif
  asm ((W {}));				// { dg-error "no matching function for call to 'W::size\\\(\\\)'" }
  asm ((X {}));				// { dg-error "no matching function for call to 'X::data\\\(\\\)'" }
  asm ((Y {}));				// { dg-error "no matching function for call to 'Y::data\\\(\\\)'" }
#if __cpp_concepts >= 201907L
  asm ((Z {}) :::);
#endif
}

template <typename TT>
void
bar ()
{
  int v1, v2;
  asm ("");
  asm ((""));			// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "request for member 'size' in '\\\(\\\"\\\"\\\)', which is of non-class type 'const char \\\[1\\\]'" "" { target *-*-* } .-1 }
  asm (("" + 0));		// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "request for member 'size' in '\\\(const char\\\*\\\)\\\"\\\"', which is of non-class type 'const char\\\*'" "" { target *-*-* } .-1 }
  asm ((0) ::);			// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "request for member 'size' in '0', which is of non-class type 'int'" "" { target *-*-* } .-1 }
  asm ("" : (A {}) (v1));	// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "'struct A' has no member named 'size'" "" { target *-*-* } .-1 }
  asm ("" : : (B {}) (1));	// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "'struct B' has no member named 'data'" "" { target *-*-* } .-1 }
  asm ("" ::: (C {}));		// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "'struct C' has no member named 'data'" "" { target *-*-* } .-1 }
  asm ((D {}));			// { dg-error "'D\\\(\\\).D::data' cannot be used as a function" }
  asm ("" : (E {}) (v1));	// { dg-error "'E\\\{0\\\}.E::size' cannot be used as a function" }
  __asm ("" :: (F {}) (1));	// { dg-error "constexpr string 'size\\\(\\\)' must be implicitly convertible to 'std::size_t'" }
				// { dg-error "could not convert 'F\\\(\\\).F::size\\\(\\\)' from 'const char\\\*' to '\[^']*'" "" { target *-*-* } .-1 }
				// { dg-error "conversion from 'const char\\\*' to '\[^']*' in a converted constant expression" "" { target *-*-* } .-2 }
  asm ("" : : : (G {}));	// { dg-error "constexpr string 'data\\\(\\\)' must be implicitly convertible to 'const char\\\*'" }
				// { dg-error "could not convert 'G\\\(\\\).G::data\\\(\\\)' from 'float' to 'const char\\\*'" "" { target *-*-* } .-1 }
  asm ("" : "=r" (v1), (H {}) (v2)); // { dg-error "call to non-'constexpr' function 'short int H::size\\\(\\\) const'" }
				// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" "" { target *-*-* } .-1 }
  asm ((I {}));			// { dg-error "call to non-'constexpr' function 'const char\\\* I::data\\\(\\\) const'" }
				// { dg-error "constexpr string 'data\\\(\\\)' must be a core constant expression" "" { target *-*-* } .-1 }

  asm ((J (0)));
  asm ("" :: (J (1)) (1));	// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" }
  asm ((M {}));
#if __cpp_constexpr_dynamic_alloc >= 201907L
  asm ((N {}));			// { dg-error "constexpr string 'data\\\(\\\)\\\[0\\\]' must be a constant expression" "" { target c++20 } }
#endif
  asm ((O {}));
  asm ((P (0)));
  asm ((P (2)));
  asm ((Q (0)));
  asm ((Q (1)));
  asm ((R (0)));
  asm ((R (2)));
  asm ((S {}));

  asm ((string_view {}));
  asm ((string_view ("test")));
  asm ((string_view ("א")));
  asm ((string_view (0, nullptr)));
  asm ((string_view (4, "testwithextrachars")));
  asm ((string_view (42, "test")));				// { dg-error "array subscript value '41' is outside the bounds of array type 'const char \\\[5\\\]'" }
								// { dg-error "constexpr string 'data\\\(\\\)\\\[41\\\]' must be a constant expression" "" { target *-*-* } .-1 }

  asm ((array<char, 2> { 'O', 'K' }));
  asm ((array<wchar_t, 2> { L'O', L'K' }));			// { dg-error "constexpr string 'data\\\(\\\)' must be implicitly convertible to 'const char\\\*'" }
								// { dg-error "could not convert 'array<wchar_t, 2>{const wchar_t \\\[2\\\]{\[0-9]+, \[0-9]+}}.array<wchar_t, 2>::data\\\(\\\)' from 'const wchar_t\\\*' to 'const char\\\*'" "" { target *-*-* } .-1 }
  asm ((array<char, 4> { 't', 'e', 's', 't' }));
  {
    constexpr auto a = array<char, 4> { 't', 'e', 's', 't' };
    asm ((a));
  }

#if __cplusplus >= 201402L
  asm ((V {}));
#endif
  asm ((W {}));				// { dg-error "no matching function for call to 'W::size\\\(\\\)'" }
  asm ((X {}));				// { dg-error "no matching function for call to 'X::data\\\(\\\)'" }
  asm ((Y {}));				// { dg-error "no matching function for call to 'Y::data\\\(\\\)'" }
#if __cpp_concepts >= 201907L
  asm ((Z {}) :::);
#endif
}

void
baz ()
{
  bar<int> ();
}

namespace NN
{
  template <typename T>
  struct A {
    constexpr int size () const = delete;
    constexpr const char *data () const { return "test"; } };
#if __cpp_concepts >= 201907L
  template <typename T>
  struct B {
    constexpr int size () const { return 4; }
    constexpr const char *data () const requires false { return "test"; } };
#endif
  class C {
    constexpr int size () const = delete;
    constexpr const char *data () const { return "test"; } };
#if __cplusplus >= 201402L
  struct D {
    constexpr int size () { return 4; }
    constexpr int size () const { return 3; }
    constexpr const char *data () { return "test"; }
    constexpr const char *data () const { return "ehlo"; } };
#endif
  struct E {
    constexpr int size () const { return 4; }
    constexpr const char *data () const { return "test"; } };
  constexpr E operator ""_myd (const char *, size_t) { return E {}; }
  constexpr E operator + (const char *, const E &) { return E {}; }
  struct H {
    static constexpr int size () { return 7; }
    static constexpr const char *data () { return "message"; } };
  struct I {
    static constexpr int size () { return 0; }
    static constexpr const char *data () { return nullptr; } };
#if __cplusplus >= 201402L
  struct J {
    static constexpr int size () { return 0; }
    static constexpr const char *data (int x = 0) { if (x) return nullptr; else throw 1; } }; // { dg-error "expression '<throw-expression>' is not a constant expression" "" { target c++14 } }
#endif
#if __cpp_if_consteval >= 202106L
  struct K {
    static constexpr int size () { if consteval { return 4; } else { throw 1; } }
    static constexpr const char *data () { return "test"; }
  };
  struct L {
    static constexpr int size () { return 4; }
    static constexpr const char *data () { if consteval { return "test"; } else { throw 1; } }
  };
  struct M {
    static constexpr int size () { if consteval { throw 1; } else { return 4; } } // { dg-error "expression '<throw-expression>' is not a constant expression" "" { target c++23 } }
    static constexpr const char *data () { return "test"; }
  };
  struct N {
    static constexpr int size () { return 4; }
    static constexpr const char *data () { if consteval { throw 1; } else { return "test"; } } // { dg-error "expression '<throw-expression>' is not a constant expression" "" { target c++23 } }
  };
#endif
  struct O { constexpr int operator () () const { return 12; } };
  struct P { constexpr const char *operator () () const { return "another test"; } };
  struct Q { O size; P data; };
  constexpr int get_size () { return 16; }
  constexpr const char *get_data () { return "yet another test"; }
  struct R { int (*size) () = NN::get_size;
	     const char *(*data) () = NN::get_data; };

  void
  bar ()
  {
    asm ((A<int> {}));		// { dg-error "use of deleted function 'constexpr int NN::A<T>::size\\\(\\\) const \\\[with T = int\\\]'" }
#if __cpp_concepts >= 201907L
    asm ((B<short> {}));	// { dg-error "no matching function for call to 'NN::B<short int>::data\\\(\\\)'" "" { target c++20 } }
#endif
    asm ((C {}));		// { dg-error "use of deleted function 'constexpr int NN::C::size\\\(\\\) const'" }
				// { dg-error "'constexpr const char\\\* NN::C::data\\\(\\\) const' is private within this context" "" { target *-*-* } .-1 }
#if __cplusplus >= 201402L
    asm ((D {}));
#endif
    asm (("foo"_myd));
    asm (("foo" + E {}));
    asm ((H {}));
    asm ((I {}));
#if __cplusplus >= 201402L
    asm ((J {}));		// { dg-error "constexpr string 'data\\\(\\\)' must be a core constant expression" "" { target c++14 } }
#endif
#if __cpp_if_consteval >= 202106L
    asm ((K {}));
    asm ((L {}));
    asm ((M {}));		// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" "" { target c++23 } }
    asm ((N {}));		// { dg-error "constexpr string 'data\\\(\\\)\\\[0\\\]' must be a constant expression" "" { target c++23 } }
#endif
    asm ((Q {}));
    asm ((R {}));
  }

  template <typename TT, typename UU, typename VV, typename WW>
  void
  baz ()
  {
    asm ((A<VV> {}));		// { dg-error "use of deleted function 'constexpr int NN::A<T>::size\\\(\\\) const \\\[with T = int\\\]'" }
#if __cpp_concepts >= 201907L
    asm ((B<WW> {}));		// { dg-error "no matching function for call to 'NN::B<short int>::data\\\(\\\)'" "" { target c++20 } }
#endif
    asm ((C {}));		// { dg-error "use of deleted function 'constexpr int NN::C::size\\\(\\\) const'" }
				// { dg-error "'constexpr const char\\\* NN::C::data\\\(\\\) const' is private within this context" "" { target *-*-* } .-1 }
#if __cplusplus >= 201402L
    asm ((D {}));
#endif
    asm (("foo"_myd));
    asm (("foo" + E {}));
    asm ((H {}));
    asm ((I {}));
#if __cplusplus >= 201402L
    asm ((J {}));		// { dg-error "constexpr string 'data\\\(\\\)' must be a core constant expression" "" { target c++14 } }
#endif
#if __cpp_if_consteval >= 202106L
    asm ((K {}));
    asm ((L {}));
    asm ((M {}));		// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" "" { target c++23 } }
    asm ((N {}));		// { dg-error "constexpr string 'data\\\(\\\)\\\[0\\\]' must be a constant expression" "" { target c++23 } }
#endif
    asm ((Q {}));
    asm ((R {}));
    asm ((TT {}));
    asm ((UU {}));		// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" }
				// { dg-error "request for member 'size' in '0', which is of non-class type 'long int'" "" { target *-*-* } .-1 }
  }
  void
  qux ()
  {
    baz<E, long, int, short> ();
  }
}
