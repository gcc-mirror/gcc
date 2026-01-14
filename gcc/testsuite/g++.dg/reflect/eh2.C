// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test throwing std::meta::exception.

#include <meta>

using namespace std::meta;

int i;
static_assert (is_reference_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_class_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_union_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_enum_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_member_function_pointer_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_member_object_pointer_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_pointer_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_array_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_void_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_null_pointer_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_integral_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_floating_point_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_lvalue_reference_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_rvalue_reference_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert (is_reflection_type (^^i)); // { dg-error "non-constant|uncaught exception" }
static_assert ((remove_const (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((remove_volatile (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((remove_cv (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((add_const (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((add_volatile (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((add_cv (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_object_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_arithmetic_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_member_pointer_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_scalar_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_fundamental_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_compound_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((remove_reference (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((add_lvalue_reference (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((add_rvalue_reference (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((make_signed (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((make_unsigned (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((remove_extent (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((remove_all_extents (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((remove_pointer (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((add_pointer (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_const_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_volatile_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_trivially_copyable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_standard_layout_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_empty_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_polymorphic_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_abstract_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_final_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_aggregate_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_consteval_only_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_signed_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_unsigned_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_bounded_array_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_unbounded_array_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_scoped_enum_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_default_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_copy_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_move_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_copy_assignable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_move_assignable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_destructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_trivially_default_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_trivially_copy_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_trivially_move_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_trivially_copy_assignable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_trivially_move_assignable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_trivially_destructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_nothrow_default_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_nothrow_copy_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_nothrow_move_constructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_nothrow_copy_assignable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_nothrow_move_assignable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_nothrow_destructible_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((has_virtual_destructor (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((has_unique_object_representations (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((rank (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((extent (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((extent (^^i, 42), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((remove_cvref (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((decay (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((underlying_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_implicit_lifetime_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_swappable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((is_nothrow_swappable_type (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((unwrap_reference (^^i), true)); // { dg-error "non-constant|uncaught exception" }
static_assert ((unwrap_ref_decay (^^i), true)); // { dg-error "non-constant|uncaught exception" }
