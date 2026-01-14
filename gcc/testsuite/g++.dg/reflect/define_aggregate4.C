// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::define_aggregate.

#include <meta>
#include <ranges>

using namespace std::meta;

constexpr auto uctx = access_context::unchecked ();
constexpr auto ctx = access_context::unprivileged ();

void
foo ()
{
  struct S;
  class C;
  union U;
  static_assert (!is_complete_type (^^S));
  static_assert (!is_complete_type (^^C));
  static_assert (!is_complete_type (^^U));
  consteval {
    if (is_complete_type (^^S))
      throw 1;
    if (is_complete_type (^^C))
      throw 2;
    if (is_complete_type (^^U))
      throw 3;
    define_aggregate (^^S, {});
    define_aggregate (^^C, {});
    define_aggregate (^^U, {});
    if (!is_complete_type (^^S))
      throw 4;
    if (!is_complete_type (^^C))
      throw 5;
    if (!is_complete_type (^^U))
      throw 6;
    if (!nonstatic_data_members_of (^^S, uctx).empty ())
      throw 7;
    if (!nonstatic_data_members_of (^^C, uctx).empty ())
      throw 8;
    if (!nonstatic_data_members_of (^^U, uctx).empty ())
      throw 9;
  }
  static_assert (is_complete_type (^^S));
  static_assert (is_complete_type (^^C));
  static_assert (is_complete_type (^^U));
  static_assert (nonstatic_data_members_of (^^S, uctx).empty ());
  static_assert (nonstatic_data_members_of (^^C, uctx).empty ());
  static_assert (nonstatic_data_members_of (^^U, uctx).empty ());
}

void
bar ()
{
  struct S;
  class C;
  union U;
  union U2;
  static_assert (!is_complete_type (^^S));
  static_assert (!is_complete_type (^^C));
  static_assert (!is_complete_type (^^U));
  consteval {
    if (is_complete_type (^^S))
      throw 1;
    if (is_complete_type (^^C))
      throw 2;
    if (is_complete_type (^^U))
      throw 3;
    define_aggregate (^^S, { data_member_spec (^^int, { .name = "a", .alignment = 32 }),
			     data_member_spec (^^bool, { .name = "b" }),
			     data_member_spec (^^int, { .bit_width = 0 }),
			     data_member_spec (^^int, { .name = "_", .bit_width = 3 }),});
    define_aggregate (^^C, { data_member_spec (^^int, { .name = "a", .alignment = 32 }),
			     data_member_spec (^^bool, { .name = "b" }),
			     data_member_spec (^^int, { .bit_width = 0 }),
			     data_member_spec (^^int, { .name = "_", .bit_width = 3 }),});
    define_aggregate (^^U, { data_member_spec (^^int, { .name = "a", .alignment = 32 }),
			     data_member_spec (^^bool, { .name = "b" }),
			     data_member_spec (^^int, { .bit_width = 0 }),
			     data_member_spec (^^int, { .name = "_", .bit_width = 3 }),});
    define_aggregate (^^U2, { data_member_spec (^^int, { .name = "a" }),
			      data_member_spec (^^bool, { .name = "b" }),
			      data_member_spec (^^int, { .bit_width = 0 }),
			      data_member_spec (^^int, { .name = "_", .bit_width = 3 }),});
    if (!is_complete_type (^^S))
      throw 4;
    if (!is_complete_type (^^C))
      throw 5;
    if (!is_complete_type (^^U))
      throw 6;
    if (!nonstatic_data_members_of (^^S, uctx).size () == 3)
      throw 7;
    if (!nonstatic_data_members_of (^^C, uctx).size () == 3)
      throw 8;
    if (!nonstatic_data_members_of (^^U, uctx).size () == 3)
      throw 9;
    if (!nonstatic_data_members_of (^^S, ctx).size () == 3)
      throw 10;
    if (!nonstatic_data_members_of (^^C, ctx).size () == 3)
      throw 11;
    if (!nonstatic_data_members_of (^^U, ctx).size () == 3)
      throw 12;
    if (!alignment_of (nonstatic_data_members_of (^^S, uctx)[0]) == 32)
      throw 13;
    if (!bit_size_of (members_of (^^S, uctx)[2]) == 0)
      throw 14;
    if (!is_bit_field (members_of (^^S, ctx)[2]))
      throw 15;
    if (!bit_size_of (members_of (^^S, uctx)[3]) == 3)
      throw 16;
    if (!is_bit_field (members_of (^^S, ctx)[3]))
      throw 17;
    if (!alignment_of (nonstatic_data_members_of (^^C, uctx)[0]) == 32)
      throw 18;
    if (!bit_size_of (members_of (^^C, uctx)[2]) == 0)
      throw 19;
    if (!is_bit_field (members_of (^^C, ctx)[2]))
      throw 20;
    if (!bit_size_of (members_of (^^C, uctx)[3]) == 3)
      throw 21;
    if (!is_bit_field (members_of (^^C, ctx)[3]))
      throw 22;
    if (!alignment_of (nonstatic_data_members_of (^^U, uctx)[0]) == 32)
      throw 23;
    if (!bit_size_of (members_of (^^U, uctx)[2]) == 0)
      throw 24;
    if (!is_bit_field (members_of (^^U, ctx)[2]))
      throw 25;
    if (!bit_size_of (members_of (^^U, uctx)[3]) == 3)
      throw 26;
    if (!is_bit_field (members_of (^^U, ctx)[3]))
      throw 27;
    if (!is_public (members_of (^^S, uctx)[0]))
      throw 28;
    if (!is_public (members_of (^^S, uctx)[1]))
      throw 29;
    if (!is_public (members_of (^^S, uctx)[2]))
      throw 30;
    if (!is_public (members_of (^^S, uctx)[3]))
      throw 31;
    if (!is_public (members_of (^^C, uctx)[0]))
      throw 32;
    if (!is_public (members_of (^^C, uctx)[1]))
      throw 33;
    if (!is_public (members_of (^^C, uctx)[2]))
      throw 34;
    if (!is_public (members_of (^^C, uctx)[3]))
      throw 35;
    if (!is_public (members_of (^^U, uctx)[0]))
      throw 36;
    if (!is_public (members_of (^^U, uctx)[1]))
      throw 37;
    if (!is_public (members_of (^^U, uctx)[2]))
      throw 38;
    if (!is_public (members_of (^^U, uctx)[3]))
      throw 39;
    if (!is_class_type (^^S))
      throw 40;
    if (!is_class_type (^^C))
      throw 41;
    if (!is_union_type (^^U))
      throw 42;
  }
  static_assert (is_complete_type (^^S));
  static_assert (is_complete_type (^^C));
  static_assert (is_complete_type (^^U));
  static_assert (nonstatic_data_members_of (^^S, uctx).size () == 3);
  static_assert (nonstatic_data_members_of (^^C, uctx).size () == 3);
  static_assert (nonstatic_data_members_of (^^U, uctx).size () == 3);
  static_assert (nonstatic_data_members_of (^^S, ctx).size () == 3);
  static_assert (nonstatic_data_members_of (^^C, ctx).size () == 3);
  static_assert (nonstatic_data_members_of (^^U, ctx).size () == 3);
  static_assert (alignment_of (nonstatic_data_members_of (^^S, uctx)[0]) == 32);
  static_assert (bit_size_of (members_of (^^S, uctx)[2]) == 0);
  static_assert (is_bit_field (members_of (^^S, ctx)[2]));
  static_assert (bit_size_of (members_of (^^S, uctx)[3]) == 3);
  static_assert (is_bit_field (members_of (^^S, ctx)[3]));
  static_assert (alignment_of (nonstatic_data_members_of (^^C, uctx)[0]) == 32);
  static_assert (bit_size_of (members_of (^^C, uctx)[2]) == 0);
  static_assert (is_bit_field (members_of (^^C, ctx)[2]));
  static_assert (bit_size_of (members_of (^^C, uctx)[3]) == 3);
  static_assert (is_bit_field (members_of (^^C, ctx)[3]));
  static_assert (alignment_of (nonstatic_data_members_of (^^U, uctx)[0]) == 32);
  static_assert (bit_size_of (members_of (^^U, uctx)[2]) == 0);
  static_assert (is_bit_field (members_of (^^U, ctx)[2]));
  static_assert (bit_size_of (members_of (^^U, uctx)[3]) == 3);
  static_assert (is_bit_field (members_of (^^U, ctx)[3]));
  static_assert (is_public (members_of (^^S, uctx)[0]));
  static_assert (is_public (members_of (^^S, uctx)[1]));
  static_assert (is_public (members_of (^^S, uctx)[2]));
  static_assert (is_public (members_of (^^S, uctx)[3]));
  static_assert (is_public (members_of (^^C, uctx)[0]));
  static_assert (is_public (members_of (^^C, uctx)[1]));
  static_assert (is_public (members_of (^^C, uctx)[2]));
  static_assert (is_public (members_of (^^C, uctx)[3]));
  static_assert (is_public (members_of (^^U, uctx)[0]));
  static_assert (is_public (members_of (^^U, uctx)[1]));
  static_assert (is_public (members_of (^^U, uctx)[2]));
  static_assert (is_public (members_of (^^U, uctx)[3]));
  static_assert (is_class_type (^^S));
  static_assert (is_class_type (^^C));
  static_assert (is_union_type (^^U));
  static_assert (sizeof (U2) == sizeof (int));
  constexpr S s = { 1, true, 2 };
  static_assert (s.a == 1 && s.b == true && s._ == 2);
  constexpr C c = { 3, false, -4 };
  static_assert (c.a == 3 && c.b == false && c._ == -4);
  union U u { .a = 42 };
  u.b = true;
  u._ = 3;
  u.[: nonstatic_data_members_of (^^U, ctx)[2] :] = 1;
}

void
baz ()
{
  struct E {};
  struct S;
  consteval {
    define_aggregate (^^S, { data_member_spec (^^int, { .name = "_" }),
			     data_member_spec (^^E, { .name = "_", .no_unique_address = true }) });
  }
  static_assert (sizeof (S) == sizeof (int));
}