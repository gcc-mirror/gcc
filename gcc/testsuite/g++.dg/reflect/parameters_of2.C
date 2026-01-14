// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::parameters_of.

#include <meta>

using namespace std::meta;

void fn0 ();
void fn1 (int);
void fn2 (int, int);
void fn3 (int, int, int);
void fn4 (int, int, int, int);
template<typename... Ts>
void tfn (Ts...);

static_assert (parameters_of (^^void()).size() == 0);
static_assert (parameters_of (^^void(void)).size() == 0);
static_assert (parameters_of (^^void(...)).size() == 0);
static_assert (parameters_of (^^void(int)).size() == 1);
static_assert (parameters_of (^^void(int, ...)).size() == 1);
static_assert (parameters_of (^^void(int, int)).size() == 2);
static_assert (parameters_of (^^void(int, int, ...)).size() == 2);
static_assert (parameters_of (^^void(int, int, int)).size() == 3);
static_assert (parameters_of (^^void(int, int, int, ...)).size() == 3);
static_assert (parameters_of (^^void(int, int, int, int)).size() == 4);
static_assert (parameters_of (^^void(int, int, int, int, ...)).size() == 4);
static_assert (parameters_of (^^void(int, int, int, int, int)).size() == 5);
static_assert (parameters_of (^^void(int, int, int, int, int, ...)).size() == 5);
static_assert (parameters_of (^^void(int, int, int, int, int, int)).size() == 6);
static_assert (parameters_of (^^void(int, int, int, int, int, int, ...)).size() == 6);
static_assert (parameters_of (^^void(int, int, int, int, int, int, int)).size() == 7);
static_assert (parameters_of (^^void(int, int, int, int, int, int, int, ...)).size() == 7);

static_assert (parameters_of (^^fn0).size() == 0);
static_assert (parameters_of (^^fn1).size() == 1);
static_assert (parameters_of (^^fn2).size() == 2);
static_assert (parameters_of (^^fn3).size() == 3);
static_assert (parameters_of (^^fn4).size() == 4);

static_assert (parameters_of (^^tfn<>).size() == 0);
