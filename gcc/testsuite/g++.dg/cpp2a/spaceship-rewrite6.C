// { dg-do compile { target c++20 } }

// We wrongly considered D to be ne_comparable because we were looking for a
// corresponding op!= for N::op== in ::, because ::op== happened to be the
// first thing in the lookup set.

template<bool, typename _Tp = void>
struct enable_if;

template<typename _Tp>
struct enable_if<true, _Tp>
{ typedef _Tp type; };

template <class T, class U> struct A { };

namespace N {
  struct X { };
  template <class T> auto operator== (const A<T,X>&, const A<T,X>&)
    -> typename enable_if<sizeof(T() == T()), bool>::type;
  template <class T> auto operator!= (const A<T,X>&, const A<T,X>&)
    -> typename enable_if<sizeof(T() != T()), bool>::type;
}

template<typename T, typename U = T>
concept ne_comparable
= requires (const A<T,N::X>& t, const A<U,N::X>& u) {
  t != u;
};

struct D { };
int operator==(D, D);
bool operator!=(D, D) = delete;
static_assert( ! ne_comparable<D> );
