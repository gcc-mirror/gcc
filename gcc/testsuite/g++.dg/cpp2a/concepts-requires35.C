// PR c++/110927
// { dg-do compile { target c++20 } }

template<class T>
struct A {
  template<class U> struct B { using type = B; };
  template<class U> using type = U;
};

template<> struct A<void> { };

template<class T>
concept C1 = requires { typename A<T>::template B<bool>::type; };

template<class T>
concept C2 = requires { typename A<T>::template B<bool>; };

template<class T>
concept C3 = requires { typename A<T>::template type<bool>; };

static_assert(C1<int>);
static_assert(C2<int>);
static_assert(C3<int>);

static_assert(!C1<void>);
static_assert(!C2<void>);
static_assert(!C3<void>);
