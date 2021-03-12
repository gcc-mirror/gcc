// PR c++/99365
// { dg-do compile { target c++20 } }

template <class> concept C = true;
template <class T, class U> concept D = C<T> && __is_same(T, U);

template <class, C auto> struct A { static const int i = 0; };
template <class T, D<T> auto V> struct A<T, V> { static const int i = 1; };

static_assert(A<int, 0>::i == 1);
static_assert(A<char, 0>::i == 0);
static_assert(A<int, '0'>::i == 0);
static_assert(A<char, '0'>::i == 1);

template <class> struct O {
  template <class, C auto> struct A { static const int i = 0; };
  template <class T, D<T> auto V> struct A<T, V> { static const int i = 1; };
};

static_assert(O<void>::A<int, 0>::i == 1);
static_assert(O<void>::A<char, 0>::i == 0);
static_assert(O<void>::A<int, '0'>::i == 0);
static_assert(O<void>::A<char, '0'>::i == 1);
