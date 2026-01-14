// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::constant_of.

#include <meta>

using namespace std::meta;

constexpr int i = 1;
static_assert (constant_of (^^i) == constant_of (object_of (^^i)));
const int &r = i;
static_assert (constant_of (^^i) == reflect_constant (1));
static_assert (object_of (^^r) == object_of (^^i));

const int constGlobal = 11;
constexpr auto rref = reflect_object (constGlobal);

static_assert (constant_of (^^constGlobal) != ^^constGlobal);
static_assert ([:constant_of (^^constGlobal):] == 11);
static_assert ([:constant_of (rref):] == 11);
static_assert (constant_of (^^constGlobal) == constant_of (rref));
static_assert (constant_of (^^constGlobal) == reflect_constant (11));

enum Enum { A };
static constexpr Enum e = A;

enum EnumCls { CA };
static constexpr EnumCls ce = CA;

static_assert (constant_of (^^A) != constant_of (^^CA));
static_assert (constant_of (^^A) != reflect_constant (0));
static_assert (constant_of (^^A) == reflect_constant (Enum (0)));

static_assert (constant_of (^^A) != reflect_constant (EnumCls (0)));
static_assert (constant_of (^^e) != reflect_constant (0));
static_assert (constant_of (^^e) == reflect_constant (Enum (0)));
static_assert (constant_of (^^e) != reflect_constant (EnumCls (0)));
static_assert (constant_of (^^ce) != reflect_constant (0));
static_assert (constant_of (^^ce) != reflect_constant (Enum (0)));
static_assert (constant_of (^^ce) == reflect_constant (EnumCls (0)));

using Alias1 = int;
constexpr Alias1 a1 = 3;
// ??? In clang++, type_of here produces "^^const int".  But
// <https://eel.is/c++draft/meta.reflection#queries-2> doesn't say anything
// about dealias.  On the other side it in all cases talks about
// reflection of types and type aliases aren't types, just entities
// with underlying entity thereof.
static_assert (type_of (^^a1) == ^^const int);
static_assert (type_of (constant_of (^^a1)) == ^^int);

struct S{};
using Alias2 = S;
constexpr Alias2 a2 {};
// And here maybe it should be ^^S instead of ^^Alias2.
static_assert (type_of (^^a2) == ^^const Alias2);
// constant_of produces _ZTAXtl1SEE whose type is "const S".
static_assert (type_of (constant_of (^^a2)) == ^^const S);

constexpr const int &ref = a1;
static_assert (type_of (constant_of (^^ref)) == ^^int);

constexpr std::pair<std::pair<int, bool>, int> p = {{1, true}, 2};
static_assert (type_of (constant_of (reflect_object (p.first))) == ^^const std::pair<int, bool>);
constexpr info rfirst = reflect_object (p.first);
static_assert (is_object (rfirst) && !is_value (rfirst));
static_assert (type_of (rfirst) == ^^const std::pair<int, bool>);
static_assert (rfirst != reflect_constant (std::make_pair (1, true)));

constexpr info rvfirst = constant_of (rfirst);
static_assert (is_object (rvfirst) && !is_value (rvfirst));
static_assert (type_of (rvfirst) == ^^const std::pair<int, bool>);
static_assert (rvfirst == reflect_constant (std::make_pair (1, true)));
static_assert ([:rvfirst:].first == 1);

constexpr int g = 3;
consteval info
fn ()
{
  const int &rg = g;
  static_assert ([:constant_of(^^rg):] == 3);
  return constant_of (^^rg);
}
static_assert ([:fn ():] == 3);

constexpr int foo () { return 42; }
static_assert (constant_of (^^foo) == reflect_function (foo));
