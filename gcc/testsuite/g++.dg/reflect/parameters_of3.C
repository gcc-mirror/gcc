// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::parameters_of.

#include <meta>

using namespace std::meta;

void lox (int = 0, int = 0, int = 0);
static_assert (parameters_of (^^lox).size() == 3);

struct C {
  ~C();
  void foo (int, bool = false);
  bool bar (this C &self, int, ...);
  static C &baz (int, ...);
  using U = void(int, int, int);
};

static_assert (parameters_of (^^C::foo).size() == 2);
static_assert (parameters_of (^^C::bar).size() == 2);
static_assert (parameters_of (^^C::baz).size() == 1);
static_assert (parameters_of (^^C::~C).size() == 0);

template<typename... Ts>
void pack (Ts...) { }
static_assert (parameters_of (^^pack<>).size() == 0);
static_assert (parameters_of (^^pack<int>).size() == 1);
static_assert (parameters_of (^^pack<int, char>).size() == 2);
static_assert (parameters_of (^^pack<int, char, float>).size() == 3);
static_assert (parameters_of (^^pack<int, char, float, unsigned>).size() == 4);
static_assert (parameters_of (^^pack<int, char, float, unsigned, short>).size() == 5);

void fn (int a, bool b);
void fn (int a, bool b = false);
void fn (int a, bool b);
static_assert (parameters_of (^^fn).size() == 2);

using A = void(int, int);
static_assert (parameters_of (dealias(^^A)).size() == 2);
static_assert (parameters_of (dealias(^^C::U)).size() == 3);
