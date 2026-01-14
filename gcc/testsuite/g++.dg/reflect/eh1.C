// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test throwing std::meta::exception.

#include <meta>

using namespace std::meta;

consteval void
eval (int n)
{
  switch (n)
    {
    case 0:
      is_reference_type (^^n);
      break;
    case 1:
      is_class_type (^^n);
      break;
    case 2:
      is_union_type (^^n);
      break;
    case 3:
      is_enum_type (^^n);
      break;
    case 4:
      is_member_function_pointer_type (^^n);
      break;
    case 5:
      is_member_object_pointer_type (^^n);
      break;
    case 6:
      is_array_type (^^n);
      break;
    case 7:
      is_pointer_type (^^n);
      break;
    case 8:
      is_void_type (^^n);
      break;
    case 9:
      is_null_pointer_type (^^n);
      break;
    case 10:
      is_integral_type (^^n);
      break;
    case 11:
      is_floating_point_type (^^n);
      break;
    case 12:
      is_lvalue_reference_type (^^n);
      break;
    case 13:
      is_rvalue_reference_type (^^n);
      break;
    case 14:
      is_reflection_type (^^n);
      break;
    case 15:
      remove_const (^^n);
      break;
    case 16:
      remove_volatile (^^n);
      break;
    case 17:
      remove_cv (^^n);
      break;
    case 18:
      add_const (^^n);
      break;
    case 19:
      add_volatile (^^n);
      break;
    case 20:
      add_cv (^^n);
      break;
    case 21:
      is_object_type (^^n);
      break;
    case 22:
      is_arithmetic_type (^^n);
      break;
    case 23:
      is_member_pointer_type (^^n);
      break;
    case 24:
      is_scalar_type (^^n);
      break;
    case 25:
      is_fundamental_type (^^n);
      break;
    case 26:
      is_compound_type (^^n);
      break;
    case 27:
      remove_reference (^^n);
      break;
    case 28:
      add_lvalue_reference (^^n);
      break;
    case 29:
      add_rvalue_reference (^^n);
      break;
    case 30:
      make_signed (^^n);
      break;
    case 31:
      make_unsigned (^^n);
      break;
    case 32:
      remove_extent (^^n);
      break;
    case 33:
      remove_all_extents (^^n);
      break;
    case 34:
      remove_pointer (^^n);
      break;
    case 35:
      add_pointer (^^n);
      break;
    case 36:
      is_const_type (^^n);
      break;
    case 37:
      is_volatile_type (^^n);
      break;
    case 38:
      is_trivially_copyable_type (^^n);
      break;
    case 41:
      is_standard_layout_type (^^n);
      break;
    case 42:
      is_empty_type (^^n);
      break;
    case 43:
      is_polymorphic_type (^^n);
      break;
    case 44:
      is_abstract_type (^^n);
      break;
    case 45:
      is_final_type (^^n);
      break;
    case 46:
      is_aggregate_type (^^n);
      break;
    case 47:
      is_consteval_only_type (^^n);
      break;
    case 48:
      is_signed_type (^^n);
      break;
    case 49:
      is_unsigned_type (^^n);
      break;
    case 50:
      is_bounded_array_type (^^n);
      break;
    case 51:
      is_unbounded_array_type (^^n);
      break;
    case 52:
      is_scoped_enum_type (^^n);
      break;
    case 53:
      is_default_constructible_type (^^n);
      break;
    case 54:
      is_copy_constructible_type (^^n);
      break;
    case 55:
      is_move_constructible_type (^^n);
      break;
    case 56:
      is_copy_assignable_type (^^n);
      break;
    case 57:
      is_move_assignable_type (^^n);
      break;
    case 58:
      is_destructible_type (^^n);
      break;
    case 59:
      is_trivially_default_constructible_type (^^n);
      break;
    case 60:
      is_trivially_copy_constructible_type (^^n);
      break;
    case 61:
      is_trivially_move_constructible_type (^^n);
      break;
    case 62:
      is_trivially_copy_assignable_type (^^n);
      break;
    case 63:
      is_trivially_move_assignable_type (^^n);
      break;
    case 64:
      is_trivially_destructible_type (^^n);
      break;
    case 65:
      is_nothrow_default_constructible_type (^^n);
      break;
    case 66:
      is_nothrow_copy_constructible_type (^^n);
      break;
    case 67:
      is_nothrow_move_constructible_type (^^n);
      break;
    case 68:
      is_nothrow_copy_assignable_type (^^n);
      break;
    case 69:
      is_nothrow_move_assignable_type (^^n);
      break;
    case 70:
      is_nothrow_destructible_type (^^n);
      break;
    case 72:
      has_virtual_destructor (^^n);
      break;
    case 73:
      has_unique_object_representations (^^n);
      break;
    case 74:
      rank (^^n);
      break;
    case 75:
      extent (^^n);
      break;
    case 76:
      extent (^^n, 2);
      break;
    case 77:
      remove_cvref (^^n);
      break;
    case 78:
      decay (^^n);
      break;
    case 79:
      underlying_type (^^n);
      break;
    case 80:
      is_implicit_lifetime_type (^^n);
      break;
    case 81:
      is_swappable_type (^^n);
      break;
    case 82:
      is_nothrow_swappable_type (^^n);
      break;
    case 83:
      unwrap_reference (^^n);
      break;
    case 84:
      unwrap_ref_decay (^^n);
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
static_assert (test (41));
static_assert (test (42));
static_assert (test (43));
static_assert (test (44));
static_assert (test (45));
static_assert (test (46));
static_assert (test (47));
static_assert (test (48));
static_assert (test (49));
static_assert (test (50));
static_assert (test (51));
static_assert (test (52));
static_assert (test (53));
static_assert (test (54));
static_assert (test (55));
static_assert (test (56));
static_assert (test (57));
static_assert (test (58));
static_assert (test (59));
static_assert (test (60));
static_assert (test (61));
static_assert (test (62));
static_assert (test (63));
static_assert (test (64));
static_assert (test (65));
static_assert (test (66));
static_assert (test (67));
static_assert (test (68));
static_assert (test (69));
static_assert (test (70));
static_assert (test (72));
static_assert (test (73));
static_assert (test (74));
static_assert (test (75));
static_assert (test (76));
static_assert (test (77));
static_assert (test (78));
static_assert (test (79));
static_assert (test (80));
static_assert (test (81));
static_assert (test (82));
static_assert (test (83));
static_assert (test (84));
