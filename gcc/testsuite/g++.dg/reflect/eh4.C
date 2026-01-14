// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test throwing std::meta::exception.

#include <meta>

using namespace std::meta;

struct S { };

consteval void
eval (int n)
{
  switch (n)
    {
    case 0:
      is_same_type (^^S, ^^n);
      break;
    case 1:
      is_same_type (^^n, ^^S);
      break;
    case 2:
      is_base_of_type (^^S, ^^n);
      break;
    case 3:
      is_base_of_type (^^n, ^^S);
      break;
    case 4:
      is_virtual_base_of_type (^^S, ^^n);
      break;
    case 5:
      is_virtual_base_of_type (^^n, ^^S);
      break;
    case 6:
      is_convertible_type (^^S, ^^n);
      break;
    case 7:
      is_convertible_type (^^n, ^^S);
      break;
    case 8:
      is_nothrow_convertible_type (^^S, ^^n);
      break;
    case 9:
      is_nothrow_convertible_type (^^n, ^^S);
      break;
    case 10:
      is_layout_compatible_type (^^S, ^^n);
      break;
    case 11:
      is_layout_compatible_type (^^n, ^^S);
      break;
    case 12:
      is_pointer_interconvertible_base_of_type (^^S, ^^n);
      break;
    case 13:
      is_pointer_interconvertible_base_of_type (^^n, ^^S);
      break;
    case 14:
      is_assignable_type (^^S, ^^n);
      break;
    case 15:
      is_assignable_type (^^n, ^^S);
      break;
    case 16:
      is_trivially_assignable_type (^^S, ^^n);
      break;
    case 17:
      is_trivially_assignable_type (^^n, ^^S);
      break;
    case 18:
      is_nothrow_assignable_type (^^S, ^^n);
      break;
    case 19:
      is_nothrow_assignable_type (^^n, ^^S);
      break;
    case 20:
      reference_constructs_from_temporary (^^S, ^^n);
      break;
    case 21:
      reference_constructs_from_temporary (^^n, ^^S);
      break;
    case 22:
      reference_converts_from_temporary (^^S, ^^n);
      break;
    case 23:
      reference_converts_from_temporary (^^n, ^^S);
      break;
    case 24:
      type_order (^^S, ^^n);
      break;
    case 25:
      type_order (^^n, ^^S);
      break;
    case 26:
      is_constructible_type (^^n, {});
      break;
    case 27:
      is_constructible_type (^^S, { ^^S, ^^n, ^^S });
      break;
    case 28:
      is_trivially_constructible_type (^^n, {});
      break;
    case 29:
      is_trivially_constructible_type (^^S, { ^^n, ^^S, ^^S, ^^S });
      break;
    case 30:
      is_nothrow_constructible_type (^^n, {});
      break;
    case 31:
      is_nothrow_constructible_type (^^S, { ^^S, ^^S, ^^S, ^^S, ^^n });
      break;
    case 32:
      is_invocable_type (^^n, {});
      break;
    case 33:
      is_invocable_type (^^S, { ^^S, ^^S, ^^n, ^^S });
      break;
    case 34:
      is_nothrow_invocable_type (^^n, {});
      break;
    case 35:
      is_nothrow_invocable_type (^^S, { ^^S, ^^n, ^^S });
      break;
    case 36:
      is_invocable_r_type (^^n, ^^S, {});
      break;
    case 37:
      is_invocable_r_type (^^S, ^^n, {});
      break;
    case 38:
      is_invocable_r_type (^^S, ^^S, { ^^S, ^^n, ^^S });
      break;
    case 39:
      is_nothrow_invocable_r_type (^^n, ^^S, {});
      break;
    case 40:
      is_nothrow_invocable_r_type (^^S, ^^n, {});
      break;
    case 41:
      is_nothrow_invocable_r_type (^^S, ^^S, { ^^S, ^^n, ^^S });
      break;
    case 42:
      invoke_result (^^n, {});
      break;
    case 43:
      invoke_result (^^S, { ^^S, ^^n, ^^S });
      break;
    case 44:
      is_swappable_with_type (^^S, ^^n);
      break;
    case 45:
      is_swappable_with_type (^^n, ^^S);
      break;
    case 46:
      is_nothrow_swappable_with_type (^^S, ^^n);
      break;
    case 47:
      is_nothrow_swappable_with_type (^^n, ^^S);
      break;
    default:
      break;
    }
}

consteval bool
test (int n)
{
  try { eval (n); }
  catch (std::meta::exception &) { return true; }
  catch (...) { return false; }
  return false;
}

static_assert (test (0));
static_assert (test (1));
static_assert (test (2));
static_assert (test (3));
static_assert (test (4));
static_assert (test (5));
static_assert (test (6));
static_assert (test (7));
static_assert (test (8));
static_assert (test (9));
static_assert (test (10));
static_assert (test (11));
static_assert (test (12));
static_assert (test (13));
static_assert (test (14));
static_assert (test (15));
static_assert (test (16));
static_assert (test (17));
static_assert (test (18));
static_assert (test (19));
static_assert (test (20));
static_assert (test (21));
static_assert (test (22));
static_assert (test (23));
static_assert (test (24));
static_assert (test (25));
static_assert (test (26));
static_assert (test (27));
static_assert (test (28));
static_assert (test (29));
static_assert (test (30));
static_assert (test (31));
static_assert (test (32));
static_assert (test (33));
static_assert (test (34));
static_assert (test (35));
static_assert (test (36));
static_assert (test (37));
static_assert (test (38));
static_assert (test (39));
static_assert (test (40));
static_assert (test (41));
static_assert (test (42));
static_assert (test (43));
static_assert (test (44));
static_assert (test (45));
static_assert (test (46));
static_assert (test (47));
