// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_noexcept.

#include <meta>

using namespace std::meta;

struct S1 {
  void foo () noexcept {
    static_assert (is_noexcept (^^foo));
  }

  void bar () {
    static_assert (!is_noexcept (^^bar));
  }
};

template<typename>
struct S2 {
  void foo () noexcept {
    static_assert (is_noexcept (^^foo));
  }

  void bar () {
    static_assert (!is_noexcept (^^bar));
  }

  template<typename>
  void tfoo () noexcept {
    static_assert (is_noexcept (^^tfoo<int>));
  }

  template<typename>
  void tbar () {
    static_assert (!is_noexcept (^^tbar<int>));
  }
};

S2<int> s2;

void fn1 ();
void fn2 () noexcept;
void fn3 () noexcept(false);

static_assert (!is_noexcept (type_of (^^::fn1)));
static_assert (is_noexcept (type_of (^^::fn2)));
static_assert (!is_noexcept (type_of (^^::fn3)));

static_assert (!is_noexcept (reflect_constant (&::fn1)));
static_assert (!is_noexcept (reflect_constant (&::fn2)));
static_assert (!is_noexcept (reflect_constant (&::fn3)));

static_assert (!is_noexcept (type_of (reflect_constant (&::fn1))));
static_assert (!is_noexcept (type_of (reflect_constant (&::fn2))));
static_assert (!is_noexcept (type_of (reflect_constant (&::fn3))));

template<typename>
void tfn1 ();
template<typename>
void tfn2 () noexcept;
template<typename>
void tfn3 () noexcept(false);

static_assert (!is_noexcept (type_of (^^::tfn1<int>)));
static_assert (is_noexcept (type_of (^^::tfn2<int>)));
static_assert (!is_noexcept (type_of (^^::tfn3<int>)));

static_assert (!is_noexcept (reflect_constant (^^::tfn1<int>)));
static_assert (!is_noexcept (reflect_constant (^^::tfn2<int>)));
static_assert (!is_noexcept (reflect_constant (^^::tfn3<int>)));

static_assert (!is_noexcept (type_of (reflect_constant (^^::tfn1<int>))));
static_assert (!is_noexcept (type_of (reflect_constant (^^::tfn2<int>))));
static_assert (!is_noexcept (type_of (reflect_constant (^^::tfn3<int>))));

struct S {
  void memfn1 ();
  void memfn2 () noexcept;
  void memfn3 () noexcept(false);

  virtual void vmemfn1 () {}
  virtual void vmemfn2 () noexcept {}
  virtual void vmemfn3 () noexcept(false) {}

  template<typename>
  void tmemfn1 ();
  template<typename>
  void tmemfn2 () noexcept;
  template<typename>
  void tmemfn3 () noexcept(false);
};

static_assert (!is_noexcept (type_of (^^S::memfn1)));
static_assert (is_noexcept (type_of (^^S::memfn2)));
static_assert (!is_noexcept (type_of (^^S::memfn3)));

static_assert (!is_noexcept (type_of (^^S::vmemfn1)));
static_assert (is_noexcept (type_of (^^S::vmemfn2)));
static_assert (!is_noexcept (type_of (^^S::vmemfn3)));

static_assert (!is_noexcept (type_of (^^S::tmemfn1<int>)));
static_assert (is_noexcept (type_of (^^S::tmemfn2<int>)));
static_assert (!is_noexcept (type_of (^^S::tmemfn3<int>)));

static_assert (!is_noexcept (reflect_constant (&S::memfn1)));
static_assert (!is_noexcept (reflect_constant (&S::memfn2)));
static_assert (!is_noexcept (reflect_constant (&S::memfn3)));

static_assert (!is_noexcept (type_of (reflect_constant (&S::memfn1))));
static_assert (!is_noexcept (type_of (reflect_constant (&S::memfn2))));
static_assert (!is_noexcept (type_of (reflect_constant (&S::memfn3))));

static_assert (!is_noexcept (reflect_constant (&S::vmemfn1)));
static_assert (!is_noexcept (reflect_constant (&S::vmemfn2)));
static_assert (!is_noexcept (reflect_constant (&S::vmemfn3)));

static_assert (!is_noexcept (type_of (reflect_constant (&S::vmemfn1))));
static_assert (!is_noexcept (type_of (reflect_constant (&S::vmemfn2))));
static_assert (!is_noexcept (type_of (reflect_constant (&S::vmemfn3))));

static_assert (!is_noexcept (reflect_constant (&S::tmemfn1<int>)));
static_assert (!is_noexcept (reflect_constant (&S::tmemfn2<int>)));
static_assert (!is_noexcept (reflect_constant (&S::tmemfn3<int>)));

static_assert (!is_noexcept (type_of (reflect_constant (&S::tmemfn1<int>))));
static_assert (!is_noexcept (type_of (reflect_constant (&S::tmemfn2<int>))));
static_assert (!is_noexcept (type_of (reflect_constant (&S::tmemfn3<int>))));

constexpr auto noexcept_lambda = []() noexcept {};
constexpr auto not_noexcept_lambda = []{};
static_assert (!is_noexcept (type_of (^^noexcept_lambda)));
static_assert (!is_noexcept (type_of (^^not_noexcept_lambda)));

constexpr auto noexcept_generic_lambda = []<typename T>() noexcept {};
constexpr auto not_noexcept_generic_lambda = []<typename T>() {};
static_assert (!is_noexcept (type_of (^^noexcept_generic_lambda)));
static_assert (!is_noexcept (type_of (^^not_noexcept_generic_lambda)));

struct T {
  static const int static_mem = 1;
  int non_static_mem = 2;
};

template <typename>
struct TT {};

enum class EC { X };
enum E { X };

static auto static_x = 1;
auto non_static_x = static_x;
auto t = T();
auto template_t = TT<int>();
int c_array[] = {1, 2};
auto [sb1, sb2] = c_array;

static_assert (!is_noexcept (^^static_x));
static_assert (!is_noexcept (type_of (^^static_x)));
static_assert (!is_noexcept (^^non_static_x));
static_assert (!is_noexcept (type_of (^^non_static_x)));
static_assert (!is_noexcept (^^T::static_mem));
static_assert (!is_noexcept (type_of (^^T::static_mem)));
static_assert (!is_noexcept (^^sb1));
static_assert (!is_noexcept (^^T::non_static_mem));
static_assert (!is_noexcept (type_of (^^T::non_static_mem)));
static_assert (!is_noexcept (^^TT));
static_assert (!is_noexcept (^^template_t));
static_assert (!is_noexcept (type_of (^^template_t)));
static_assert (!is_noexcept (^^EC));
static_assert (!is_noexcept (^^EC::X));
static_assert (!is_noexcept (type_of (^^EC::X)));
static_assert (!is_noexcept (^^E));
static_assert (!is_noexcept (^^X));
static_assert (!is_noexcept (type_of (^^X)));
