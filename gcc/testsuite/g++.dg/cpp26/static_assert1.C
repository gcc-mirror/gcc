// C++26 P2741R3 - user-generated static_assert messages
// { dg-do compile { target c++11 } }
// { dg-options "" }
// Override any default-'-fno-exceptions':
// { dg-additional-options -fexceptions }

static_assert (true, "");
static_assert (true, (""));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
				// { dg-error "request for member 'size' in '\\\(\\\"\\\"\\\)', which is of non-class type 'const char \\\[1\\\]'" "" { target *-*-* } .-2 }
static_assert (true, "" + 0);	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
				// { dg-error "request for member 'size' in '\\\(const char\\\*\\\)\\\"\\\"', which is of non-class type 'const char\\\*'" "" { target *-*-* } .-2 }
static_assert (true, 0);	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
				// { dg-error "request for member 'size' in '0', which is of non-class type 'int'" "" { target *-*-* } .-2 }
struct A {};
static_assert (true, A {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
				// { dg-error "'struct A' has no member named 'size'" "" { target *-*-* } .-2 }
struct B { int size; };
static_assert (true, B {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
				// { dg-error "'struct B' has no member named 'data'" "" { target *-*-* } .-2 }
struct C { constexpr int size () const { return 0; } };
static_assert (true, C {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
				// { dg-error "'struct C' has no member named 'data'" "" { target *-*-* } .-2 }
struct D { constexpr int size () const { return 0; } int data; };
static_assert (true, D {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "'D\\\(\\\).D::data' cannot be used as a function" "" { target *-*-* } .-1 }
struct E { int size = 0;
	   constexpr const char *data () const { return ""; } };
static_assert (true, E {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "'E\\\(\\\).E::size' cannot be used as a function" "" { target c++11_only } .-1 }
				// { dg-error "'E\\\{0\\\}.E::size' cannot be used as a function" "" { target c++14 } .-2 }
struct F { constexpr const char *size () const { return ""; }
	   constexpr const char *data () const { return ""; } };
static_assert (true, F {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string 'size\\\(\\\)' must be implicitly convertible to 'std::size_t'" "" { target *-*-* } .-1 }
				// { dg-error "could not convert 'F\\\(\\\).F::size\\\(\\\)' from 'const char\\\*' to '\[^']*'" "" { target *-*-* } .-2 }
				// { dg-error "conversion from 'const char\\\*' to '\[^']*' in a converted constant expression" "" { target *-*-* } .-3 }
struct G { constexpr long size () const { return 0; }
	   constexpr float data () const { return 0.0f; } };
static_assert (true, G {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string 'data\\\(\\\)' must be implicitly convertible to 'const char\\\*'" "" { target *-*-* } .-1 }
				// { dg-error "could not convert 'G\\\(\\\).G::data\\\(\\\)' from 'float' to 'const char\\\*'" "" { target *-*-* } .-2 }
struct H { short size () const { return 0; }
	   constexpr const char *data () const { return ""; } };
static_assert (true, H {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
struct I { constexpr signed char size () const { return 0; }
	   const char *data () const { return ""; } };
static_assert (true, I {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
struct J { constexpr int size () const { return j ? throw 1 : 0; }	// { dg-error "expression '<throw-expression>' is not a constant expression" }
	   constexpr const char *data () const { return ""; };
	   constexpr J (int x) : j (x) {}
	   int j; };
static_assert (true, J (1));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
static_assert (false, J (0));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed" "" { target *-*-* } .-1 }
static_assert (false, J (1));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" "" { target *-*-* } .-1 }
struct K { constexpr operator int () { return 4; } };
struct L { constexpr operator const char * () { return "test"; } };
struct M { constexpr K size () const { return {}; }
	   constexpr L data () const { return {}; } };
static_assert (true, M {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
static_assert (false, M {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
#if  __cpp_constexpr_dynamic_alloc >= 201907L
struct N { constexpr int size () const { return 3; }
	   constexpr const char *data () const { return new char[3] { 'b', 'a', 'd' }; } }; // { dg-error "'\\\* N\\\(\\\).N::data\\\(\\\)' is not a constant expression because allocated storage has not been deallocated" "" { target c++20 } }
static_assert (true, N {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++20 && c++23_down } } }
static_assert (false, N {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++20 && c++23_down } } }
				// { dg-error "constexpr string 'data\\\(\\\)\\\[0\\\]' must be a constant expression" "" { target c++20 } .-1 }
#endif
constexpr const char a[] = { 't', 'e', 's', 't' };
struct O { constexpr int size () const { return 4; }
	   constexpr const char *data () const { return a; } };
static_assert (false, O {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
struct P { constexpr int size () const { return 4 - p; }
	   constexpr const char *data () const { return &a[p]; }
	   constexpr P (int x) : p (x) {}
	   int p; };
static_assert (false, P (0));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
static_assert (false, P (2));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: st" "" { target *-*-* } .-1 }
struct Q { constexpr int size () const { return 4 - q; }
	   constexpr const char *data () const { return &"test"[q]; }
	   constexpr Q (int x) : q (x) {}
	   int q; };
static_assert (false, Q (0));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
static_assert (false, Q (1));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: est" "" { target *-*-* } .-1 }
struct R { constexpr int size () const { return 4 - r; }
	   constexpr const char *d () const { return "test"; }
	   constexpr const char *data () const { return d () + r; }
	   constexpr R (int x) : r (x) {}
	   int r; };
static_assert (false, R (0));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
static_assert (false, R (2));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: st" "" { target *-*-* } .-1 }
struct S { constexpr float size (float) const { return 42.0f; }
	   constexpr int size (void * = nullptr) const { return 4; }
	   constexpr double data (double) const { return 42.0; }
	   constexpr const char *data (int = 0) const { return "test"; } };
static_assert (true, S {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
static_assert (false, S {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }

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
static_assert (true, string_view{});				// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
static_assert (false, string_view ("test"));			// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
								// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
static_assert (false, string_view ("א"));			// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
								// { dg-error "static assertion failed: א" "" { target *-*-* } .-1 }
static_assert (false, string_view (0, nullptr));		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
								// { dg-error "static assertion failed" "" { target *-*-* } .-1 }
static_assert (false, string_view (4, "testwithextrachars"));	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
								// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
static_assert (false, string_view (42, "test"));		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
								// { dg-error "array subscript value '41' is outside the bounds of array type 'const char \\\[5\\\]'" "" { target *-*-* } .-1 }
								// { dg-error "constexpr string 'data\\\(\\\)\\\[41\\\]' must be a constant expression" "" { target *-*-* } .-2 }

template <typename T, size_t N>
struct array {
  constexpr size_t size () const { return N; }
  constexpr const T *data () const { return a; }
  const T a[N];
};
static_assert (true, array<char, 2> { 'O', 'K' });		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
static_assert (true, array<wchar_t, 2> { L'O', L'K' });		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
								// { dg-error "constexpr string 'data\\\(\\\)' must be implicitly convertible to 'const char\\\*'" "" { target *-*-* } .-1 }
								// { dg-error "could not convert 'array<wchar_t, 2>{const wchar_t \\\[2\\\]{\[0-9]+, \[0-9]+}}.array<wchar_t, 2>::data\\\(\\\)' from 'const wchar_t\\\*' to 'const char\\\*'" "" { target *-*-* } .-2 }
static_assert (false, array<char, 4> { 't', 'e', 's', 't' });	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
								// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }

void
foo ()
{
  constexpr auto a = array<char, 4> { 't', 'e', 's', 't' };
  static_assert (false, a);					// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
}								// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }

#if  __cpp_constexpr_dynamic_alloc >= 201907L
struct T {
  const char *d = init ();
  constexpr int size () const { return 4; }
  constexpr const char *data () const { return d; }
  constexpr const char *init () const { return new char[4] { 't', 'e', 's', 't' }; }
  constexpr ~T () { delete[] d; }
};
static_assert (false, T{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++20 && c++23_down } } }
					// { dg-error "static assertion failed: test" "" { target c++20 } .-1 }
#endif
struct U { constexpr operator const char * () const { return u; }
	   char u[5] = "test"; };
#if __cplusplus >= 201402L
struct V { constexpr auto size () const { return K{}; }
	   constexpr auto data () const { return U{}; } };
static_assert (false, V{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++14 && c++23_down } } }
					// { dg-error "static assertion failed: test" "" { target c++14 } .-1 }
#endif
struct W { constexpr int size (int) const { return 4; }
	   constexpr const char *data () const { return "test"; } };
static_assert (true, W{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "no matching function for call to 'W::size\\\(\\\)'" "" { target *-*-* } .-1 }
struct X { constexpr int size () const { return 4; }
	   constexpr const char *data (int) const { return "test"; } };
static_assert (true, X{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "no matching function for call to 'X::data\\\(\\\)'" "" { target *-*-* } .-1 }
struct Y { constexpr int size () { return 4; }
	   constexpr const char *data (int) { return "test"; } };
static_assert (true, Y{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "no matching function for call to 'Y::data\\\(\\\)'" "" { target *-*-* } .-1 }
#if __cpp_concepts >= 201907L
struct Z { constexpr int size (auto...) const { return 4; }
	   constexpr const char *data (auto...) const { return "test"; } };
static_assert (false, Z{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++20 && c++23_down } } }
					// { dg-error "static assertion failed: test" "" { target c++20 } .-1 }
#endif

namespace NN
{
  template <typename T>
  struct A {
    constexpr int size () const = delete;
    constexpr const char *data () const { return "test"; } };
  static_assert (true, A<int>{});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "use of deleted function 'constexpr int NN::A<T>::size\\\(\\\) const \\\[with T = int\\\]'" "" { target *-*-* } .-1 }
#if __cpp_concepts >= 201907L
  template <typename T>
  struct B {
    constexpr int size () const { return 4; }
    constexpr const char *data () const requires false { return "test"; } };
  static_assert (true, B<short>{});	// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++20 && c++23_down } } }
					// { dg-error "no matching function for call to 'NN::B<short int>::data\\\(\\\)'" "" { target c++20 } .-1 }
#endif
  class C {
    constexpr int size () const = delete;
    constexpr const char *data () const { return "test"; } };
  static_assert (true, C{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "use of deleted function 'constexpr int NN::C::size\\\(\\\) const'" "" { target *-*-* } .-1 }
					// { dg-error "'constexpr const char\\\* NN::C::data\\\(\\\) const' is private within this context" "" { target *-*-* } .-2 }
#if __cplusplus >= 201402L
  struct D {
    constexpr int size () { return 4; }
    constexpr int size () const { return 3; }
    constexpr const char *data () { return "test"; }
    constexpr const char *data () const { return "ehlo"; } };
  static_assert (true, D{});	// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++14 && c++23_down } } }
  static_assert (false, D{});	// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++14 && c++23_down } } }
				// { dg-error "static assertion failed: test" "" { target c++14 } .-1 }
#endif
  struct E {
    constexpr int size () const { return 4; }
    constexpr const char *data () const { return "test"; } };
  template <typename T>
  struct F {
    static_assert (false, T{});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
  };				// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
  template <typename T>
  struct G {
    static_assert (false, T{});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
  };				// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
				// { dg-error "request for member 'size' in '0', which is of non-class type 'long int'" "" { target *-*-* } .-2 }
  F<E> fe;
  G<long> gl;
  constexpr E operator ""_myd (const char *, size_t) { return E{}; }
  static_assert (false, "foo"_myd);	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
  constexpr E operator + (const char *, const E &) { return E{}; }
  static_assert (false, "foo" + E{});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "static assertion failed: test" "" { target *-*-* } .-1 }
  struct H {
    static constexpr int size () { return 7; }
    static constexpr const char *data () { return "message"; } };
  static_assert (true, H{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
  static_assert (false, H{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "static assertion failed: message" "" { target *-*-* } .-1 }
  struct I {
    static constexpr int size () { return 0; }
    static constexpr const char *data () { return nullptr; } };
  static_assert (true, I{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
  static_assert (false, I{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
					// { dg-error "static assertion failed" "" { target *-*-* } .-1 }
#if __cplusplus >= 201402L
  struct J {
    static constexpr int size () { return 0; }
    static constexpr const char *data (int x = 0) { if (x) return nullptr; else throw 1; } }; // { dg-error "expression '<throw-expression>' is not a constant expression" "" { target c++14 } }
  static_assert (true, J{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++14 && c++23_down } } }
  static_assert (false, J{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target { c++14 && c++23_down } } }
					// { dg-error "constexpr string 'data\\\(\\\)' must be a core constant expression" "" { target c++14 } .-1 }
#endif
#if __cpp_if_consteval >= 202106L
  struct K {
    static constexpr int size () { if consteval { return 4; } else { throw 1; } }
    static constexpr const char *data () { return "test"; }
  };
  static_assert (true, K{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_only } }
  static_assert (false, K{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_only } }
					// { dg-error "static assertion failed: test" "" { target c++23 } .-1 }
  struct L {
    static constexpr int size () { return 4; }
    static constexpr const char *data () { if consteval { return "test"; } else { throw 1; } }
  };
  static_assert (true, L{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_only } }
  static_assert (false, L{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_only } }
					// { dg-error "static assertion failed: test" "" { target c++23 } .-1 }
  struct M {
    static constexpr int size () { if consteval { throw 1; } else { return 4; } } // { dg-error "expression '<throw-expression>' is not a constant expression" "" { target c++23 } }
    static constexpr const char *data () { return "test"; }
  };
  static_assert (true, M{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_only } }
  static_assert (false, M{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_only } }
					// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" "" { target c++23 } .-1 }
  struct N {
    static constexpr int size () { return 4; }
    static constexpr const char *data () { if consteval { throw 1; } else { return "test"; } } // { dg-error "expression '<throw-expression>' is not a constant expression" "" { target c++23 } }
  };
  static_assert (true, N{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_only } }
  static_assert (false, N{});		// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_only } }
					// { dg-error "constexpr string 'data\\\(\\\)\\\[0\\\]' must be a constant expression" "" { target c++23 } .-1 }
#endif
  struct O { constexpr int operator () () const { return 12; } };
  struct P { constexpr const char *operator () () const { return "another test"; } };
  struct Q { O size; P data; };
  static_assert (true, Q ());	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
  static_assert (false, Q {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: another test" "" { target *-*-* } .-1 }
  constexpr int get_size () { return 16; }
  constexpr const char *get_data () { return "yet another test"; }
  struct R { int (*size) () = NN::get_size;
	     const char *(*data) () = NN::get_data; };
  static_assert (true, R ());	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
  static_assert (false, R {});	// { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "static assertion failed: yet another test" "" { target *-*-* } .-1 }
}
