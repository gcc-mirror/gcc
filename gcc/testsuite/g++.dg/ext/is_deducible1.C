// { dg-do compile { target c++20 } }

#if __has_builtin (__is_deducible)

template <class T> struct A { };
template <class T> struct B { };

// Simple forms.
static_assert (__is_deducible (::A, A<int>));
static_assert (__is_deducible (B, B<int>));
static_assert (!__is_deducible (A, B<int>));
static_assert (!__is_deducible (::B, A<int>));

// This is the interesting use case for alias CTAD.
template <class T> using AP = A<T*>;
static_assert (__is_deducible (AP, A<int*>));
static_assert (!__is_deducible (AP, A<int>));

// Can't deduce a parameter not used on the RHS.
template <class T> using C = void;
static_assert (!__is_deducible (C, C<int>));

// But a default template argument counts.
template <class T = void> using D = void;
static_assert (__is_deducible (D, D<int>));

// P0127 array bound type deduction should work here.
template <class T, T N> using E = int[N];
static_assert (__is_deducible (E, int[42]));

#endif // __has_builtin (__is_deducible)

// We don't try to support this.
template <class T> void f(T);
bool b = __is_deducible (f, void (int)); // { dg-error "" }
