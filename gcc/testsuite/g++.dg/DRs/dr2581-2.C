// DR 2581 - Undefined behavior for predefined macros
// { dg-do preprocess }
// { dg-additional-options "-fcontracts" { target c++26 } }
// { dg-additional-options "-fmodules -fcoroutines" { target c++20 } }

#define defined defined				// { dg-error "'defined' cannot be used as a macro name" }
#define __cplusplus 202400L			// { dg-error "'__cplusplus' redefined" }
#define __DATE__				// { dg-error "'__DATE__' redefined" }
#define __FILE__				// { dg-error "'__FILE__' redefined" }
#define __LINE__				// { dg-error "'__LINE__' redefined" }
#define __STDC_EMBED_NOT_FOUND__ 0		// { dg-error "'__STDC_EMBED_NOT_FOUND__' redefined" }
#define __STDC_EMBED_FOUND__ 1			// { dg-error "'__STDC_EMBED_FOUND__' redefined" }
#define __STDC_EMBED_EMPTY__ 2			// { dg-error "'__STDC_EMBED_EMPTY__' redefined" }
#define __STDC_HOSTED__	1			// { dg-error "'__STDC_HOSTED__' redefined" }
#define __STDCPP_DEFAULT_NEW_ALIGNMENT__ 1	// { dg-error "'__STDCPP_DEFAULT_NEW_ALIGNMENT__' redefined" "" { target c++17 } }
#define __STDCPP_FLOAT16_T__ 1			// { dg-message "'__STDCPP_FLOAT16_T__' (?:re)?defined" "" { target c++23 } } 
#define __STDCPP_FLOAT32_T__ 1			// { dg-message "'__STDCPP_FLOAT32_T__' (?:re)?defined" "" { target c++23 } } 
#define __STDCPP_FLOAT64_T__ 1			// { dg-message "'__STDCPP_FLOAT64_T__' (?:re)?defined" "" { target c++23 } } 
#define __STDCPP_FLOAT128_T__ 1			// { dg-message "'__STDCPP_FLOAT128_T__' (?:re)?defined" "" { target c++23 } } 
#define __STDCPP_BFLOAT16_T__ 1			// { dg-message "'__STDCPP_BFLOAT16_T__' (?:re)?defined" "" { target c++23 } } 
#define __TIME__				// { dg-error "'__TIME__' redefined" }
#define __STDC__				// { dg-error "'__STDC__' redefined" }
#define __STDC_MB_MIGHT_NEQ_WC__		// { dg-warning "'__STDC_MB_MIGHT_NEQ_WC__' defined" "" { target c++11 } }
#define __STDC_VERSION__			// { dg-warning "'__STDC_VERSION__' defined" "" { target { c++11 && { ! *-*-solaris2* } } } }
						// { dg-error "'__STDC_VERSION__' redefined" "" { target *-*-solaris2* } .-1 }
#define __STDC_ISO_10646__			// { dg-error "'__STDC_ISO_10646__' redefined" "" { xfail { ! *-*-linux* } } }
#define __STDCPP_THREADS__			// { dg-message "'__STDCPP_THREADS__' (?:re)?defined" "" { target c++11 } }
#define __STDCPP_STRICT_POINTER_SAFETY__	// { dg-warning "'__STDCPP_STRICT_POINTER_SAFETY__' defined" "" { target { c++11 && c++20_down } } }
#define __cpp_aggregate_bases 201603L		// { dg-error "'__cpp_aggregate_bases' redefined" "" { target c++20 } }
#define __cpp_aggregate_nsdmi 201304L		// { dg-error "'__cpp_aggregate_nsdmi' redefined" "" { target c++20 } }
#define __cpp_aggregate_paren_init 201902L	// { dg-error "'__cpp_aggregate_paren_init' redefined" "" { target c++20 } }
#define __cpp_alias_templates 200704L		// { dg-error "'__cpp_alias_templates' redefined" "" { target c++20 } }
#define __cpp_aligned_new 201606L		// { dg-error "'__cpp_aligned_new' redefined" "" { target c++20 } }
#define __cpp_attributes 200809L		// { dg-error "'__cpp_attributes' redefined" "" { target c++20 } }
#define __cpp_auto_cast 202110L			// { dg-error "'__cpp_auto_cast' redefined" "" { target c++23 } }
#define __cpp_binary_literals 201304L		// { dg-error "'__cpp_binary_literals' redefined" "" { target c++20 } }
#define __cpp_capture_star_this 201603L		// { dg-error "'__cpp_capture_star_this' redefined" "" { target c++20 } }
#define __cpp_char8_t 202207L			// { dg-error "'__cpp_char8_t' redefined" "" { target c++20 } }
#define __cpp_concepts 202002L			// { dg-error "'__cpp_concepts' redefined" "" { target c++20 } }
#define __cpp_conditional_explicit 201806L	// { dg-error "'__cpp_conditional_explicit' redefined" "" { target c++20 } }
#define __cpp_constexpr 202406L			// { dg-error "'__cpp_constexpr' redefined" "" { target c++11 } }
#define __cpp_constexpr_dynamic_alloc 201907L	// { dg-error "'__cpp_constexpr_dynamic_alloc' redefined" "" { target c++20 } }
#define __cpp_constexpr_exceptions 202411L	// { dg-error "'__cpp_constexpr_exceptions' redefined" "" { target c++26 } }
#define __cpp_constexpr_in_decltype 201711L	// { dg-error "'__cpp_constexpr_in_decltype' redefined" "" { target c++20 } }
#define __cpp_constexpr_virtual_inheritance 202506L // { dg-error "'__cpp_constexpr_virtual_inheritance' redefined" "" { target c++26 } }
#define __cpp_consteval 202211L			// { dg-error "'__cpp_consteval' redefined" "" { target c++20 } }
#define __cpp_constinit 201907L			// { dg-error "'__cpp_constinit' redefined" "" { target c++20 } }
#define __cpp_contracts 202502L			// { dg-error "'__cpp_contracts' redefined" "" { target c++26 } }
#define __cpp_decltype 200707L			// { dg-error "'__cpp_decltype' redefined" "" { target c++20 } }
#define __cpp_decltype_auto 201304L		// { dg-error "'__cpp_decltype_auto' redefined" "" { target c++20 } }
#define __cpp_deduction_guides 201907L		// { dg-error "'__cpp_deduction_guides' redefined" "" { target c++17 } }
#define __cpp_delegating_constructors 200604L	// { dg-error "'__cpp_delegating_constructors' redefined" "" { target c++20 } }
#define __cpp_deleted_function 202403L		// { dg-error "'__cpp_deleted_function' redefined" "" { target c++26 } }
#define __cpp_designated_initializers 201707L	// { dg-error "'__cpp_designated_initializers' redefined" "" { target c++20 } }
#define __cpp_enumerator_attributes 201411L	// { dg-error "'__cpp_enumerator_attributes' redefined" "" { target c++20 } }
#define __cpp_expansion_statements 202506L	// { dg-error "'__cpp_expansion_statements' redefined" "" { target c++26 } }
#define __cpp_explicit_this_parameter 202110L	// { dg-error "'__cpp_explicit_this_parameter' redefined" "" { target c++23 } }
#define __cpp_fold_expressions 201603L		// { dg-error "'__cpp_fold_expressions' redefined" "" { target c++20 } }
#define __cpp_generic_lambdas 201707L		// { dg-error "'__cpp_generic_lambdas' redefined" "" { target c++14 } }
#define __cpp_guaranteed_copy_elision 201606L	// { dg-error "'__cpp_guaranteed_copy_elision' redefined" "" { target c++20 } }
#define __cpp_hex_float 201603L			// { dg-error "'__cpp_hex_float' redefined" "" { target c++20 } }
#define __cpp_if_consteval 202106L		// { dg-error "'__cpp_if_consteval' redefined" "" { target c++23 } }
#define __cpp_if_constexpr 201606L		// { dg-error "'__cpp_if_constexpr' redefined" "" { target c++20 } }
#define __cpp_impl_coroutine 201902L		// { dg-error "'__cpp_impl_coroutine' redefined" "" { target c++20 } }
#define __cpp_impl_destroying_delete 201806L	// { dg-error "'__cpp_impl_destroying_delete' redefined" "" { target c++20 } }
#define __cpp_impl_three_way_comparison 201907L	// { dg-error "'__cpp_impl_three_way_comparison' redefined" "" { target c++20 } }
#define __cpp_impl_reflection 202506L
#define __cpp_implicit_move 202207L		// { dg-error "'__cpp_implicit_move' redefined" "" { target c++23 } }
#define __cpp_inheriting_constructors 201511L	// { dg-error "'__cpp_inheriting_constructors' redefined" "" { target c++20 } }
#define __cpp_init_captures 201803L		// { dg-error "'__cpp_init_captures' redefined" "" { target c++14 } }
#define __cpp_initializer_lists 200806L		// { dg-error "'__cpp_initializer_lists' redefined" "" { target c++20 } }
#define __cpp_inline_variables 201606L		// { dg-error "'__cpp_inline_variables' redefined" "" { target c++20 } }
#define __cpp_lambdas 200907L			// { dg-error "'__cpp_lambdas' redefined" "" { target c++20 } }
#define __cpp_modules 201907L			// { dg-error "'__cpp_modules' redefined" "" { target c++20 } }
#define __cpp_multidimensional_subscript 202211L // { dg-error "'__cpp_multidimensional_subscript' redefined" "" { target c++23 } }
#define __cpp_named_character_escapes 202207L	// { dg-error "'__cpp_named_character_escapes' redefined" "" { target c++23 } }
#define __cpp_namespace_attributes 201411L	// { dg-error "'__cpp_namespace_attributes' redefined" "" { target c++20 } }
#define __cpp_noexcept_function_type 201510L	// { dg-error "'__cpp_noexcept_function_type' redefined" "" { target c++20 } }
#define __cpp_nontype_template_args 201911L	// { dg-error "'__cpp_nontype_template_args' redefined" "" { target c++17 } }
#define __cpp_nontype_template_parameter_auto 201606L // { dg-error "'__cpp_nontype_template_parameter_auto' redefined" "" { target c++20 } }
#define __cpp_nsdmi 200809L			// { dg-error "'__cpp_nsdmi' redefined" "" { target c++20 } }
#define __cpp_pack_indexing 202311L		// { dg-error "'__cpp_pack_indexing' redefined" "" { target c++26 } }
#define __cpp_placeholder_variables 202306L	// { dg-error "'__cpp_placeholder_variables' redefined" "" { target c++26 } }
#define __cpp_pp_embed 202502L			// { dg-error "'__cpp_pp_embed' redefined" "" { target c++26 } }
#define __cpp_range_based_for 202211L		// { dg-error "'__cpp_range_based_for' redefined" "" { target c++11 } }
#define __cpp_raw_strings 200710L		// { dg-error "'__cpp_raw_strings' redefined" "" { target c++20 } }
#define __cpp_ref_qualifiers 200710L		// { dg-error "'__cpp_ref_qualifiers' redefined" "" { target c++20 } }
#define __cpp_return_type_deduction 201304L	// { dg-error "'__cpp_return_type_deduction' redefined" "" { target c++20 } }
#define __cpp_rvalue_references 200610L		// { dg-error "'__cpp_rvalue_references' redefined" "" { target c++20 } }
#define __cpp_size_t_suffix 202011L		// { dg-error "'__cpp_size_t_suffix' redefined" "" { target c++23 } }
#define __cpp_sized_deallocation 201309L	// { dg-error "'__cpp_sized_deallocation' redefined" "" { target c++20 } }
#define __cpp_static_assert 202306L		// { dg-error "'__cpp_static_assert' redefined" "" { target c++11 } }
#define __cpp_static_call_operator 202207L	// { dg-error "'__cpp_static_call_operator' redefined" "" { target c++23 } }
#define __cpp_structured_bindings 202411L	// { dg-error "'__cpp_structured_bindings' redefined" "" { target c++17 } }
#define __cpp_template_parameters 202502L
#define __cpp_template_template_args 201611L	// { dg-error "'__cpp_template_template_args' redefined" "" { target c++20 } }
#define __cpp_threadsafe_static_init 200806L	// { dg-error "'__cpp_threadsafe_static_init' redefined" "" { target c++20 } }
#define __cpp_trivial_union 202502L
#define __cpp_unicode_characters 200704L	// { dg-error "'__cpp_unicode_characters' redefined" "" { target c++17 } }
#define __cpp_unicode_literals 200710L		// { dg-error "'__cpp_unicode_literals' redefined" "" { target c++20 } }
#define __cpp_user_defined_literals 200809L	// { dg-error "'__cpp_user_defined_literals' redefined" "" { target c++20 } }
#define __cpp_using_enum 201907L		// { dg-error "'__cpp_using_enum' redefined" "" { target c++20 } }
#define __cpp_variable_templates 201304L	// { dg-error "'__cpp_variable_templates' redefined" "" { target c++20 } }
#define __cpp_variadic_friend 202403L		// { dg-error "'__cpp_variadic_friend' redefined" "" { target c++26 } }
#define __cpp_variadic_templates 200704L	// { dg-error "'__cpp_variadic_templates' redefined" "" { target c++20 } }
#define __cpp_variadic_using 201611L		// { dg-error "'__cpp_variadic_using' redefined" "" { target c++20 } }
