// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::source_location_of.

#include <meta>

using namespace std::meta;

static_assert (source_location_of (^^::).line () == std::source_location ().line ());
static_assert (source_location_of (^^int).line () == std::source_location ().line ());
struct S {};
static_assert (source_location_of (^^S).line () == std::source_location::current ().line () - 1);
int x;
static_assert (source_location_of (^^x).line () == std::source_location::current ().line () - 1);
enum E {
  E1,
  E2
};
static_assert (source_location_of (^^E).line () == std::source_location::current ().line () - 4);
static_assert (source_location_of (^^E1).line () == std::source_location::current ().line () - 4);
static_assert (source_location_of (^^E2).line () == std::source_location::current ().line () - 4);
union U {};
static_assert (source_location_of (^^U).line () == std::source_location::current ().line () - 1);
void foo (int, int) {}
static_assert (source_location_of (^^foo).line () == std::source_location::current ().line () - 1);
namespace N { namespace O {} }
static_assert (source_location_of (^^N).line () == std::source_location::current ().line () - 1);
static_assert (source_location_of (^^N::O).line () == std::source_location::current ().line () - 2);
using I = int;
static_assert (source_location_of (^^I).line () == std::source_location::current ().line () - 1);
static_assert (source_location_of (dealias (^^I)).line () == std::source_location ().line ());
typedef S J;
static_assert (source_location_of (dealias (^^J)).line () == source_location_of (^^S).line ());
static_assert (source_location_of (data_member_spec (^^int, { .name = "_" })).line () == std::source_location ().line ());
constexpr auto ctx = access_context::current ();
struct V {};
struct Y { int a, b, c; };
struct W : [[=1]] public S, [[=2, =Y { 42, 42, 42 }]] virtual V {};
static_assert (source_location_of (bases_of (^^W, ctx)[0]).line () == std::source_location::current ().line () - 1);
static_assert (source_location_of (bases_of (^^W, ctx)[1]).line () == std::source_location::current ().line () - 2);
static_assert (source_location_of (annotations_of (bases_of (^^W, ctx)[0])[0]).line () == std::source_location::current ().line () - 3);
static_assert (source_location_of (annotations_of (bases_of (^^W, ctx)[1])[1]).line () == std::source_location::current ().line () - 4);
struct X : public S,
	   [[=3]] public V
{
};
static_assert (source_location_of (bases_of (^^X, ctx)[0]).line () == std::source_location::current ().line () - 4);
// TODO: we don't track location of base specifiers, so just location of the derived class definition is used.
static_assert (source_location_of (bases_of (^^X, ctx)[1]).line () == std::source_location::current ().line () - 6);
static_assert (source_location_of (annotations_of (bases_of (^^X, ctx)[1])[0]).line () == std::source_location::current ().line () - 6);
[[=1]]
[[=2]] extern int an;
[[=Y { 1, 2, 3 }]] extern int an;
[[=3.0f + 4.0f]] int an;
static_assert (source_location_of (annotations_of (^^an)[0]).line () == std::source_location::current ().line () - 4);
static_assert (source_location_of (annotations_of (^^an)[1]).line () == std::source_location::current ().line () - 4);
static_assert (source_location_of (annotations_of (^^an)[2]).line () == std::source_location::current ().line () - 4);
static_assert (source_location_of (annotations_of (^^an)[3]).line () == std::source_location::current ().line () - 4);

#include <string_view>

int baz (int x, int y);
static_assert (source_location_of (parameters_of (^^baz)[0]).line () == std::source_location::current ().line () - 1);

void
bar (int a, int b)
{
  static_assert (source_location_of (^^a).line ()
		 == std::source_location::current ().line () - 3);
  static_assert (source_location_of (^^b).line ()
		 == source_location_of (^^a).line ());
  static_assert (std::string_view (source_location_of (^^bar).file_name ())
		 == std::string_view (std::source_location::current ().file_name ()));
  static_assert (std::string_view (source_location_of (^^a).function_name ())
		 == std::string_view (std::source_location::current ().function_name ()));
  static_assert (std::string_view (source_location_of (^^b).function_name ())
		 == std::string_view (std::source_location::current ().function_name ()));
  int c;
  static_assert (source_location_of (^^c).line ()
		 == std::source_location::current ().line () - 2);
  static_assert (std::string_view (source_location_of (^^c).file_name ())
		 == std::string_view (std::source_location::current ().file_name ()));
  static_assert (std::string_view (source_location_of (^^a).function_name ())
		 == std::string_view (std::source_location::current ().function_name ()));
  constexpr auto d = parameters_of (^^bar)[0];
  static_assert (source_location_of (d).line ()
		 == source_location_of (^^a).line ());
  static_assert (std::string_view (source_location_of (d).file_name ())
		 == std::string_view (source_location_of (^^a).file_name ()));
  static_assert (std::string_view (source_location_of (d).function_name ())
		 == std::string_view (source_location_of (^^a).function_name ()));
}

template <int N>
void
qux ()
{
  [[=1]]
  [[=N + 42]]
  [[=Y { N, N + 1, N + 2 }]]
  [[=Y { 1, 2, 3 }]] int an;
  static_assert (N && source_location_of (annotations_of (^^an)[0]).line () == std::source_location::current ().line () - 4);
  static_assert (N && source_location_of (annotations_of (^^an)[1]).line () == std::source_location::current ().line () - 4);
//  static_assert (N && source_location_of (annotations_of (^^an)[2]).line () == std::source_location::current ().line () - 4);
//  static_assert (N && source_location_of (annotations_of (^^an)[3]).line () == std::source_location::current ().line () - 4);
}

void
plugh ()
{
  qux <1> ();
  qux <2> ();
}
