// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_same_type.

#include <meta>

using namespace std::meta;

using U = int;
using UU = U;

static_assert (is_same_type (^^int, ^^int));
static_assert (is_same_type (^^int, ^^U));
static_assert (is_same_type (^^int, ^^UU));
static_assert (!is_same_type (^^int, ^^const int));
static_assert (!is_same_type (^^int, ^^const UU));
static_assert (!is_same_type (^^int *, ^^int[]));
static_assert (!is_same_type (^^int&, ^^int&&));

struct incomplete;
struct B {};
struct D : B {};
struct E1 : D {};
struct E2 : D {};
static_assert (is_base_of_type (^^B, ^^D));
static_assert (is_base_of_type (^^D, ^^E1));
static_assert (is_base_of_type (^^B, ^^E1));
static_assert (is_base_of_type (^^B, ^^B));
static_assert (!is_base_of_type (^^D, ^^B));
static_assert (!is_base_of_type (^^E1, ^^D));
static_assert (!is_base_of_type (^^E1, ^^B));
static_assert (!is_base_of_type (^^B, ^^int));
static_assert (!is_base_of_type (^^incomplete, ^^D));

struct V : virtual E1, virtual E2 {};
static_assert (!is_virtual_base_of_type (^^B, ^^D));
static_assert (!is_virtual_base_of_type (^^const B, ^^D));
static_assert (!is_virtual_base_of_type (^^D, ^^B));
static_assert (!is_virtual_base_of_type (^^D, ^^int));
static_assert (!is_virtual_base_of_type (^^B, ^^B));
static_assert (is_virtual_base_of_type (^^B, ^^V));
static_assert (is_virtual_base_of_type (^^D, ^^V));
static_assert (is_virtual_base_of_type (^^E1, ^^V));
static_assert (is_virtual_base_of_type (^^E2, ^^V));

struct D1 : private virtual B {};
struct D2 : protected virtual B {};
struct D3 : private D1 {};
struct D4 : private D2 {};
struct D5 : protected D2 {};
struct D6 : protected D2 {};
static_assert (is_virtual_base_of_type (^^B, ^^D1));
static_assert (is_virtual_base_of_type (^^B, ^^D2));
static_assert (is_virtual_base_of_type (^^B, ^^D3));
static_assert (is_virtual_base_of_type (^^B, ^^D4));
static_assert (is_virtual_base_of_type (^^B, ^^D5));
static_assert (is_virtual_base_of_type (^^B, ^^D6));

struct W : V {};
struct Y : virtual V {};
static_assert (is_virtual_base_of_type (^^E1, ^^W));
static_assert (is_virtual_base_of_type (^^E2, ^^W));
static_assert (is_virtual_base_of_type (^^E1, ^^Y));
static_assert (is_virtual_base_of_type (^^E2, ^^Y));

struct X {};
struct Z { Z(int); };
struct Z2 { Z2(int) noexcept; };
static_assert (is_convertible_type (^^bool, ^^int));
static_assert (is_convertible_type (^^int, ^^double));
static_assert (!is_convertible_type (^^int, ^^X));
static_assert (is_convertible_type (^^int, ^^Z));
static_assert (!is_convertible_type (^^Z, ^^int));

static_assert (is_nothrow_convertible_type (^^bool, ^^int));
static_assert (is_nothrow_convertible_type (^^int, ^^double));
static_assert (!is_nothrow_convertible_type (^^int, ^^X));
static_assert (!is_nothrow_convertible_type (^^int, ^^Z));
static_assert (!is_nothrow_convertible_type (^^Z, ^^int));
static_assert (is_nothrow_convertible_type (^^int, ^^Z2));
static_assert (!is_nothrow_convertible_type (^^Z2, ^^int));

static_assert (is_layout_compatible_type (^^void, ^^void));
static_assert (is_layout_compatible_type (^^int, ^^int));
static_assert (!is_layout_compatible_type (^^int, ^^int[]));
static_assert (!is_layout_compatible_type (^^int, ^^int[1]));
static_assert (is_layout_compatible_type (^^int[], ^^int[]));
static_assert (is_layout_compatible_type (^^int[1], ^^int[1]));
static_assert (!is_layout_compatible_type (^^int[1], ^^int[]));
static_assert (!is_layout_compatible_type (^^int[1], ^^int[2]));
static_assert (is_layout_compatible_type (^^incomplete[], ^^incomplete[]));

namespace LC {
  enum E1 : int { };
  enum E2 : int;
  static_assert (is_layout_compatible_type (^^E1, ^^E2));
  enum E3 : unsigned int;
  static_assert (!is_layout_compatible_type (^^E1, ^^E3));
  enum E4 : char { };
  enum E5 : signed char { };
  enum E6 : unsigned char { };
  static_assert (!is_layout_compatible_type (^^E4, ^^E5));
  static_assert (!is_layout_compatible_type (^^E4, ^^E6));
  static_assert (!is_layout_compatible_type (^^E5, ^^E6));
  struct A { int a; };
  struct B { const int b; };
  static_assert (is_layout_compatible_type (^^A, ^^B));
  static_assert (is_layout_compatible_type (^^B, ^^A));
  struct C : A { };
  struct D : B { };
  static_assert (is_layout_compatible_type (^^C, ^^D));
  struct E : A { int i; };
  static_assert (!is_layout_compatible_type (^^E, ^^A));
}

namespace PI {
  struct B { };
  static_assert (is_pointer_interconvertible_base_of_type (^^B, ^^B));
  static_assert (is_pointer_interconvertible_base_of_type (^^B, ^^const B));
  static_assert (is_pointer_interconvertible_base_of_type (^^const B, ^^B));
  static_assert (is_pointer_interconvertible_base_of_type (^^const B, ^^const B));
  struct D : B { int i; };
  static_assert (is_pointer_interconvertible_base_of_type (^^D, ^^D));
  static_assert (is_pointer_interconvertible_base_of_type (^^B, ^^D));
  static_assert (is_pointer_interconvertible_base_of_type (^^const B, ^^D));
  static_assert (is_pointer_interconvertible_base_of_type (^^B, ^^const D));
  static_assert (is_pointer_interconvertible_base_of_type (^^const B, ^^const D));
  static_assert (!is_pointer_interconvertible_base_of_type (^^D, ^^B));
  struct E : D { };
  static_assert (!is_pointer_interconvertible_base_of_type (^^E, ^^B));
  struct D1 : B { };
  struct D2 : B { };
  struct D3 : D1, D2 { };
  static_assert (!is_pointer_interconvertible_base_of_type (^^B, ^^D3));
  union U;
  static_assert (!is_pointer_interconvertible_base_of_type (^^U, ^^U));
  static_assert (!is_pointer_interconvertible_base_of_type (^^U, ^^D));
  struct I;
  static_assert (is_pointer_interconvertible_base_of_type (^^I, ^^I));
  static_assert (is_pointer_interconvertible_base_of_type (^^I, ^^const I));
}
