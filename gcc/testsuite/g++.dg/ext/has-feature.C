// { dg-do compile }
// { dg-options "" }

#define FEAT(x) (__has_feature(x) && __has_extension(x))
#define CXX11 (__cplusplus >= 201103L)
#define CXX14 (__cplusplus >= 201402L)

#if FEAT(cxx_exceptions) != !!__cpp_exceptions
#error
#endif

#if FEAT(cxx_rtti) != !!__cpp_rtti
#error
#endif

#if __has_feature (cxx_access_control_sfinae) != CXX11
#error
#endif

#if !__has_extension (cxx_access_control_sfinae)
#error
#endif

#if FEAT(cxx_alias_templates) != CXX11
#error
#endif

#if FEAT(cxx_alignas) != CXX11
#error
#endif

#if FEAT(cxx_alignof) != CXX11
#error
#endif

#if FEAT(cxx_attributes) != CXX11
#error
#endif

#if FEAT(cxx_constexpr) != CXX11
#error
#endif

#if FEAT(cxx_decltype) != CXX11
#error
#endif

#if FEAT(cxx_decltype_incomplete_return_types) != CXX11
#error
#endif

#if FEAT(cxx_default_function_template_args) != CXX11
#error
#endif

#if FEAT(cxx_defaulted_functions) != CXX11
#error
#endif

#if __has_feature (cxx_delegating_constructors) != CXX11
#error
#endif

#if FEAT (cxx_deleted_functions) != CXX11
#error
#endif

#if __has_feature (cxx_explicit_conversions) != CXX11
#error
#endif

#if FEAT (cxx_generalized_initializers) != CXX11
#error
#endif

#if FEAT (cxx_implicit_moves) != CXX11
#error
#endif

#if FEAT (cxx_inheriting_constructors) != CXX11
#error
#endif

#if !__has_extension (cxx_inline_namespaces)
#error
#endif

#if __has_feature (cxx_inline_namespaces) != CXX11
#error
#endif

#if FEAT (cxx_lambdas) != CXX11
#error
#endif

#if FEAT (cxx_local_type_template_args) != CXX11
#error
#endif

#if FEAT (cxx_noexcept) != CXX11
#error
#endif

#if __has_feature (cxx_nonstatic_member_init) != CXX11
#error
#endif

#if FEAT (cxx_nullptr) != CXX11
#error
#endif

#if __has_feature (cxx_override_control) != CXX11
#error
#endif

#if FEAT (cxx_reference_qualified_functions) != CXX11
#error
#endif

#if FEAT (cxx_range_for) != CXX11
#error
#endif

#if FEAT (cxx_raw_string_literals) != CXX11
#error
#endif

#if FEAT (cxx_rvalue_references) != CXX11
#error
#endif

#if FEAT (cxx_static_assert) != CXX11
#error
#endif

#if FEAT (cxx_thread_local) != CXX11
#error
#endif

#if FEAT (cxx_auto_type) != CXX11
#error
#endif

#if FEAT (cxx_strong_enums) != CXX11
#error
#endif

#if FEAT (cxx_trailing_return) != CXX11
#error
#endif

#if FEAT (cxx_unicode_literals) != CXX11
#error
#endif

#if FEAT (cxx_unrestricted_unions) != CXX11
#error
#endif

#if FEAT (cxx_user_literals) != CXX11
#error
#endif

#if !__has_extension (cxx_variadic_templates)
#error
#endif

#if __has_feature (cxx_variadic_templates) != CXX11
#error
#endif

#if !__has_extension (cxx_binary_literals)
#error
#endif

#if __has_feature (cxx_binary_literals) != CXX14
#error
#endif

#if FEAT (cxx_decltype_auto) != CXX14
#error
#endif

#if FEAT (cxx_aggregate_nsdmi) != CXX14
#error
#endif

#if __has_extension (cxx_init_captures) != CXX11
#error
#endif

#if __has_feature (cxx_init_captures) != CXX14
#error
#endif

#if FEAT (cxx_generic_lambdas) != CXX14
#error
#endif

#if FEAT (cxx_relaxed_constexpr) != CXX14
#error
#endif

#if FEAT (cxx_return_type_deduction) != CXX14
#error
#endif

#if __has_feature (cxx_variable_templates) != CXX14
#error
#endif
