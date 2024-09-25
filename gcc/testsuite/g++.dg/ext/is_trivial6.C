// PR c++/85723
// { dg-do compile { target c++20 } }

template<typename T>
struct A {
  A() = delete;
  A() requires(sizeof(T) == 1) = default;
  A() requires(sizeof(T) != 1) = delete;
};
static_assert(!__is_trivial(A<int>));
static_assert(__is_trivial(A<char>));

template<typename T>
struct B {
  B() = default;
  B() requires(sizeof(T) == 1) = default;
  B() requires(sizeof(T) != 1) = delete;
};
static_assert(__is_trivial(B<int>));
static_assert(__is_trivial(B<char>));

template<typename T>
struct C {
  C() = default;
  C() requires(sizeof(T) == 1) = delete;
  C() requires(sizeof(T) != 1) = default;
};
static_assert(__is_trivial(C<int>));
static_assert(__is_trivial(C<char>));

template<typename T>
struct D {
  D() = default;
  D(int = 42) {}
  D() requires(sizeof(T) == 1) = delete;
  D() requires(sizeof(T) != 1) = default;
};
static_assert(!__is_trivial(D<int>));
static_assert(!__is_trivial(D<char>));


template<typename T>
struct E {
  E() = delete;
  E() requires(sizeof(T) == 1) = default;
  E() requires(sizeof(T) != 1) = delete;
};
static_assert(!__is_trivial(E<int>));
static_assert(__is_trivial(E<char>));
