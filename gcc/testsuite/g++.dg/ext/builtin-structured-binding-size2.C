// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct A;
struct B { int a; };
struct C { int a; };
struct D { int a; };
union E { int a; long b; };
struct F : public B { int c; };
struct G { struct { int d; }; int e; };
struct H { union { int f; long g; }; int h; };
struct I { private: int i; };
struct J { int b; };
struct K : public B, J {};
template <>
struct std::tuple_size <C> { static constexpr double value = 42; };
template <>
struct std::tuple_size <D> { static constexpr int value = -1; };
int a = __builtin_structured_binding_size (A);			// { dg-error "structured binding refers to incomplete class type 'A'" }
int b = __builtin_structured_binding_size (A &);		// { dg-error "'__builtin_structured_binding_size' argument 'A\\\&' is a reference" }
int c = __builtin_structured_binding_size (B[]);		// { dg-error "cannot decompose array of unknown bound 'B \\\[\\\]'" }
int d = __builtin_structured_binding_size (C);			// { dg-error "'std::tuple_size<C>::value' is not an integral constant expression" }
int e = __builtin_structured_binding_size (D);			// { dg-error "'std::tuple_size<D>::value' is not an integral constant expression" }
int f = __builtin_structured_binding_size (E);			// { dg-error "cannot decompose union type 'E'" }
int g = __builtin_structured_binding_size (float);		// { dg-error "cannot decompose non-array non-class type 'float'" }
int h = __builtin_structured_binding_size (void);		// { dg-error "cannot decompose non-array non-class type 'void'" }
int i = __builtin_structured_binding_size (long &);		// { dg-error "'__builtin_structured_binding_size' argument 'long int\\\&' is a reference" }
int j = __builtin_structured_binding_size (long *);		// { dg-error "cannot decompose non-array non-class type 'long int\\\*'" }
auto k = []() {};
int l = __builtin_structured_binding_size (decltype (k));	// { dg-error "cannot decompose lambda closure type '<lambda\\\(\\\)>'" }
int m = __builtin_structured_binding_size (F);			// { dg-error "cannot decompose class type 'F': both it and its base class 'B' have non-static data members" }
int n = __builtin_structured_binding_size (G);			// { dg-error "cannot decompose class type 'G' because it has an anonymous struct member" }
int o = __builtin_structured_binding_size (H);			// { dg-error "cannot decompose class type 'H' because it has an anonymous union member" }
int p = __builtin_structured_binding_size (I);			// { dg-error "cannot decompose inaccessible member 'I::i' of 'I'" }
int q = __builtin_structured_binding_size (K);			// { dg-error "cannot decompose class type 'K': its base classes 'B' and 'J' have non-static data members" }
static_assert (__builtin_structured_binding_size (int[0]) == 0);
void foo (int r[10], int s = __builtin_structured_binding_size (decltype (r))); // { dg-error "cannot decompose non-array non-class type 'int\\\*'" }

template <typename T, int N = __builtin_structured_binding_size (T)> // { dg-error "cannot decompose non-array non-class type 'int'" }
// { dg-error "'std::tuple_size<C>::value' is not an integral constant expression" "" { target *-*-* } .-1 }
struct L {
  static constexpr int value = N;
};
L<int> l1;							// { dg-error "template argument 2 is invalid" }
static_assert (L<B>::value == 1, "");
L<C> l2;							// { dg-error "template argument 2 is invalid" }
