// { dg-do compile { target c++11 } }

template <typename To, typename From>
constexpr To
bit_cast (const From &from)
{
  return __builtin_bit_cast (To, from);
}
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'U' is a union type" "U" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'const U' is a union type" "const U" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'B' contains a union type" "B" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'char\\\*' is a pointer type" "char ptr" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'const int\\\*' is a pointer type" "const int ptr" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'C' contains a pointer type" "C" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'const C' contains a pointer type" "const C" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'int D::\\\*' is a pointer to member type" "ptrmem 1" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'int \\\(D::\\\*\\\)\\\(\\\) const' is a pointer to member type" "ptrmem 2" { target *-*-* } 7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'int \\\(D::\\\*\\\)\\\(\\\)' is a pointer to member type" "ptrmem 3" { target *-*-* } 7 }

union U { int u; };
struct A { int a; U b; };
struct B : public A { int c; };
struct C { const int *p; };
constexpr int a[] = { 1, 2, 3 };
constexpr const int *b = &a[0];
constexpr C c = { b };
struct D { int d; constexpr int foo () const { return 1; } };
constexpr int D::*d = &D::d;
constexpr int (D::*e) () const = &D::foo;
struct E { __INTPTR_TYPE__ e, f; };
constexpr E f = { 1, 2 };
constexpr U g { 0 };

constexpr auto z = bit_cast <U> (0);
constexpr auto y = bit_cast <int> (g);
constexpr auto x = bit_cast <B> (a);
constexpr auto w = bit_cast <char *> ((__INTPTR_TYPE__) 0);
constexpr auto v = bit_cast <__UINTPTR_TYPE__> (b);
constexpr auto u = bit_cast <C> ((__INTPTR_TYPE__) 0);
constexpr auto t = bit_cast <__INTPTR_TYPE__> (c);
constexpr auto s = bit_cast <__INTPTR_TYPE__> (d);
constexpr auto r = bit_cast <E> (e);
constexpr auto q = bit_cast <int D::*> ((__INTPTR_TYPE__) 0);
constexpr auto p = bit_cast <int (D::*) ()> (f);
