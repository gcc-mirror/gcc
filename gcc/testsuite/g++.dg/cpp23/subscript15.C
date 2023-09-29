// PR c++/111493
// { dg-do compile { target c++23 } }

template<class T, class... Ts>
concept CartesianIndexable = requires(T t, Ts... ts) { t[ts...]; };

static_assert(!CartesianIndexable<int>);
static_assert(!CartesianIndexable<int, int>);
static_assert(!CartesianIndexable<int, int, int>);

static_assert(!CartesianIndexable<int*>);
static_assert(CartesianIndexable<int*, int>);
static_assert(!CartesianIndexable<int*, int, int>);
static_assert(!CartesianIndexable<int*, int*>);

template<class... Ts>
struct A {
  void operator[](Ts...);
};

static_assert(!CartesianIndexable<A<>, int>);
static_assert(CartesianIndexable<A<int>, int>);
static_assert(!CartesianIndexable<A<int>>);
static_assert(!CartesianIndexable<A<int>, int, int>);
static_assert(CartesianIndexable<A<int, int>, int, int>);
