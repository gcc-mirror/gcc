// PR c++/51222
// { dg-options -std=c++11 }

template<class T>
struct add_rref {
  typedef T&& type;
};

template<>
struct add_rref<void> {
  typedef void type;
};

template<class T>
typename add_rref<T>::type declval();

template<class T, class U, class =
  decltype(::delete ::new T(declval<U>()))
>
auto f(int) -> char;

template<class, class>
auto f(...) -> char(&)[2];

template<class T, class =
  decltype(::delete ::new T())
>
auto g(int) -> char;

template<class>
auto g(...) -> char(&)[2];

template<class T, class U>
auto f2(int) -> decltype(::delete ::new T(declval<U>()), char());

template<class, class>
auto f2(...) -> char(&)[2];

template<class T>
auto g2(int) -> decltype(::delete ::new T(), char());

template<class>
auto g2(...) -> char(&)[2];

struct C { };

struct A {
  virtual ~A() = 0;
};

struct D1 {
  D1() = delete;
};

struct D2 {
  ~D2() = delete;
};

static_assert(sizeof(g<void>(0)) == 2, "Ouch");
static_assert(sizeof(g<void()>(0)) == 2, "Ouch");
static_assert(sizeof(g<void() const>(0)) == 2, "Ouch");
static_assert(sizeof(g<A>(0)) == 2, "Ouch");
static_assert(sizeof(g<D1>(0)) == 2, "Ouch");
static_assert(sizeof(g<D2>(0)) == 2, "Ouch");
static_assert(sizeof(g<int&>(0)) == 2, "Ouch");
static_assert(sizeof(g<int&&>(0)) == 2, "Ouch");
static_assert(sizeof(g<void(&)()>(0)) == 2, "Ouch");
static_assert(sizeof(g<void(&&)()>(0)) == 2, "Ouch");
static_assert(sizeof(f<void, void>(0)) == 2, "Ouch");
static_assert(sizeof(f<void(), void()>(0)) == 2, "Ouch");
static_assert(sizeof(f<void() const, void() const>(0)) == 2, "Ouch");
static_assert(sizeof(f<int, void>(0)) == 2, "Ouch");
static_assert(sizeof(f<void, int>(0)) == 2, "Ouch");
static_assert(sizeof(f<C, void>(0)) == 2, "Ouch");
static_assert(sizeof(f<C, int>(0)) == 2, "Ouch");
static_assert(sizeof(f<int&, int&>(0)) == 2, "Ouch");
static_assert(sizeof(f<int&&, int&&>(0)) == 2, "Ouch");
static_assert(sizeof(f<void(&)(), void(&)()>(0)) == 2, "Ouch");
static_assert(sizeof(f<void(&&)(), void(&&)()>(0)) == 2, "Ouch");

static_assert(sizeof(g2<void>(0)) == 2, "Ouch");
static_assert(sizeof(g2<void()>(0)) == 2, "Ouch");
static_assert(sizeof(g2<void() const>(0)) == 2, "Ouch");
static_assert(sizeof(g2<A>(0)) == 2, "Ouch");
static_assert(sizeof(g2<D1>(0)) == 2, "Ouch");
static_assert(sizeof(g2<D2>(0)) == 2, "Ouch");
static_assert(sizeof(g2<int&>(0)) == 2, "Ouch");
static_assert(sizeof(g2<int&&>(0)) == 2, "Ouch");
static_assert(sizeof(g2<void(&)()>(0)) == 2, "Ouch");
static_assert(sizeof(g2<void(&&)()>(0)) == 2, "Ouch");
static_assert(sizeof(f2<void, void>(0)) == 2, "Ouch");
static_assert(sizeof(f2<void(), void()>(0)) == 2, "Ouch");
static_assert(sizeof(f2<void() const, void() const>(0)) == 2, "Ouch");
static_assert(sizeof(f2<int, void>(0)) == 2, "Ouch");
static_assert(sizeof(f2<void, int>(0)) == 2, "Ouch");
static_assert(sizeof(f2<C, void>(0)) == 2, "Ouch");
static_assert(sizeof(f2<C, int>(0)) == 2, "Ouch");
static_assert(sizeof(f2<int&, int&>(0)) == 2, "Ouch");
static_assert(sizeof(f2<int&&, int&&>(0)) == 2, "Ouch");
static_assert(sizeof(f2<void(&)(), void(&)()>(0)) == 2, "Ouch");
static_assert(sizeof(f2<void(&&)(), void(&&)()>(0)) == 2, "Ouch");
