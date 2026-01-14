// DR 2581 - Undefined behavior for predefined macros
// { dg-do preprocess }
// { dg-additional-options "-fcontracts -freflection" { target c++26 } }
// { dg-additional-options "-fmodules -fcoroutines" { target c++20 } }

#undef defined				// { dg-error "'defined' cannot be used as a macro name" }
#undef __cplusplus			// { dg-warning "undefining '__cplusplus'" }
#undef __DATE__				// { dg-warning "undefining '__DATE__'" }
#undef __FILE__				// { dg-warning "undefining '__FILE__'" }
#undef __LINE__				// { dg-warning "undefining '__LINE__'" }
#undef __STDC_EMBED_NOT_FOUND__		// { dg-warning "undefining '__STDC_EMBED_NOT_FOUND__'" }
#undef __STDC_EMBED_FOUND__		// { dg-warning "undefining '__STDC_EMBED_FOUND__'" }
#undef __STDC_EMBED_EMPTY__		// { dg-warning "undefining '__STDC_EMBED_EMPTY__'" }
#undef __STDC_HOSTED__			// { dg-warning "undefining '__STDC_HOSTED__'" }
#undef __STDCPP_DEFAULT_NEW_ALIGNMENT__	// { dg-warning "undefining '__STDCPP_DEFAULT_NEW_ALIGNMENT__'" "" { target c++17 } }
#undef __STDCPP_FLOAT16_T__		// { dg-warning "undefining '__STDCPP_FLOAT16_T__'" "" { target c++23 } }
#undef __STDCPP_FLOAT32_T__		// { dg-warning "undefining '__STDCPP_FLOAT32_T__'" "" { target c++23 } }
#undef __STDCPP_FLOAT64_T__		// { dg-warning "undefining '__STDCPP_FLOAT64_T__'" "" { target c++23 } }
#undef __STDCPP_FLOAT128_T__		// { dg-warning "undefining '__STDCPP_FLOAT128_T__'" "" { target c++23 } }
#undef __STDCPP_BFLOAT16_T__		// { dg-warning "undefining '__STDCPP_BFLOAT16_T__'" "" { target c++23 } }
#undef __TIME__				// { dg-warning "undefining '__TIME__'" }
#undef __STDC__				// { dg-warning "undefining '__STDC__'" }
#undef __STDC_MB_MIGHT_NEQ_WC__		// { dg-warning "undefining '__STDC_MB_MIGHT_NEQ_WC__'" "" { target c++11 } }
#undef __STDC_VERSION__			// { dg-warning "undefining '__STDC_VERSION__'" "" { target c++11 } }
#undef __STDC_ISO_10646__		// { dg-warning "undefining '__STDC_ISO_10646__'" "" { xfail { ! *-*-linux* } } }
#undef __STDCPP_THREADS__		// { dg-warning "undefining '__STDCPP_THREADS__'" "" { target c++11 } }
#undef __STDCPP_STRICT_POINTER_SAFETY__	// { dg-warning "undefining '__STDCPP_STRICT_POINTER_SAFETY__'" "" { target { c++11 && c++20_down } } }
#undef __cpp_aggregate_bases		// { dg-warning "undefining '__cpp_aggregate_bases'" "" { target c++20 } }
#undef __cpp_aggregate_nsdmi		// { dg-warning "undefining '__cpp_aggregate_nsdmi'" "" { target c++20 } }
#undef __cpp_aggregate_paren_init	// { dg-warning "undefining '__cpp_aggregate_paren_init'" "" { target c++20 } }
#undef __cpp_alias_templates		// { dg-warning "undefining '__cpp_alias_templates'" "" { target c++20 } }
#undef __cpp_aligned_new		// { dg-warning "undefining '__cpp_aligned_new'" "" { target c++20 } }
#undef __cpp_attributes			// { dg-warning "undefining '__cpp_attributes'" "" { target c++20 } }
#undef __cpp_auto_cast			// { dg-warning "undefining '__cpp_auto_cast'" "" { target c++23 } }
#undef __cpp_binary_literals		// { dg-warning "undefining '__cpp_binary_literals'" "" { target c++20 } }
#undef __cpp_capture_star_this		// { dg-warning "undefining '__cpp_capture_star_this'" "" { target c++20 } }
#undef __cpp_char8_t			// { dg-warning "undefining '__cpp_char8_t'" "" { target c++20 } }
#undef __cpp_concepts			// { dg-warning "undefining '__cpp_concepts'" "" { target c++20 } }
#undef __cpp_conditional_explicit	// { dg-warning "undefining '__cpp_conditional_explicit'" "" { target c++20 } }
#undef __cpp_constexpr			// { dg-warning "undefining '__cpp_constexpr'" "" { target c++20 } }
#undef __cpp_constexpr_dynamic_alloc	// { dg-warning "undefining '__cpp_constexpr_dynamic_alloc'" "" { target c++20 } }
#undef __cpp_constexpr_exceptions	// { dg-warning "undefining '__cpp_constexpr_exceptions'" "" { target c++26 } }
#undef __cpp_constexpr_in_decltype	// { dg-warning "undefining '__cpp_constexpr_in_decltype'" "" { target c++20 } }
#undef __cpp_constexpr_virtual_inheritance // { dg-warning "undefining '__cpp_constexpr_virtual_inheritance'" "" { target c++26 } }
#undef __cpp_consteval			// { dg-warning "undefining '__cpp_consteval'" "" { target c++20 } }
#undef __cpp_constinit			// { dg-warning "undefining '__cpp_constinit'" "" { target c++20 } }
#undef __cpp_contracts			// { dg-warning "undefining '__cpp_contracts'" "" { target c++26 } }
#undef __cpp_decltype			// { dg-warning "undefining '__cpp_decltype'" "" { target c++20 } }
#undef __cpp_decltype_auto		// { dg-warning "undefining '__cpp_decltype_auto'" "" { target c++20 } }
#undef __cpp_deduction_guides		// { dg-warning "undefining '__cpp_deduction_guides'" "" { target c++20 } }
#undef __cpp_delegating_constructors	// { dg-warning "undefining '__cpp_delegating_constructors'" "" { target c++20 } }
#undef __cpp_deleted_function		// { dg-warning "undefining '__cpp_deleted_function'" "" { target c++26 } }
#undef __cpp_designated_initializers	// { dg-warning "undefining '__cpp_designated_initializers'" "" { target c++20 } }
#undef __cpp_enumerator_attributes	// { dg-warning "undefining '__cpp_enumerator_attributes'" "" { target c++20 } }
#undef __cpp_expansion_statements	// { dg-warning "undefining '__cpp_expansion_statements'" "" { target c++26 } }
#undef __cpp_explicit_this_parameter	// { dg-warning "undefining '__cpp_explicit_this_parameter'" "" { target c++23 } }
#undef __cpp_fold_expressions		// { dg-warning "undefining '__cpp_fold_expressions'" "" { target c++20 } }
#undef __cpp_generic_lambdas		// { dg-warning "undefining '__cpp_generic_lambdas'" "" { target c++20 } }
#undef __cpp_guaranteed_copy_elision	// { dg-warning "undefining '__cpp_guaranteed_copy_elision'" "" { target c++20 } }
#undef __cpp_hex_float			// { dg-warning "undefining '__cpp_hex_float'" "" { target c++20 } }
#undef __cpp_if_consteval		// { dg-warning "undefining '__cpp_if_consteval'" "" { target c++23 } }
#undef __cpp_if_constexpr		// { dg-warning "undefining '__cpp_if_constexpr'" "" { target c++20 } }
#undef __cpp_impl_coroutine		// { dg-warning "undefining '__cpp_impl_coroutine'" "" { target c++20 } }
#undef __cpp_impl_destroying_delete	// { dg-warning "undefining '__cpp_impl_destroying_delete'" "" { target c++20 } }
#undef __cpp_impl_three_way_comparison	// { dg-warning "undefining '__cpp_impl_three_way_comparison'" "" { target c++20 } }
#undef __cpp_impl_reflection		// { dg-warning "undefining '__cpp_impl_reflection'" "" { target c++26 } }
#undef __cpp_implicit_move		// { dg-warning "undefining '__cpp_implicit_move'" "" { target c++23 } }
#undef __cpp_inheriting_constructors	// { dg-warning "undefining '__cpp_inheriting_constructors'" "" { target c++20 } }
#undef __cpp_init_captures		// { dg-warning "undefining '__cpp_init_captures'" "" { target c++20 } }
#undef __cpp_initializer_lists		// { dg-warning "undefining '__cpp_initializer_lists'" "" { target c++20 } }
#undef __cpp_inline_variables		// { dg-warning "undefining '__cpp_inline_variables'" "" { target c++20 } }
#undef __cpp_lambdas			// { dg-warning "undefining '__cpp_lambdas'" "" { target c++20 } }
#undef __cpp_modules			// { dg-warning "undefining '__cpp_modules'" "" { target c++20 } }
#undef __cpp_multidimensional_subscript	// { dg-warning "undefining '__cpp_multidimensional_subscript'" "" { target c++23 } }
#undef __cpp_named_character_escapes	// { dg-warning "undefining '__cpp_named_character_escapes'" "" { target c++23 } }
#undef __cpp_namespace_attributes	// { dg-warning "undefining '__cpp_namespace_attributes'" "" { target c++20 } }
#undef __cpp_noexcept_function_type	// { dg-warning "undefining '__cpp_noexcept_function_type'" "" { target c++20 } }
#undef __cpp_nontype_template_args	// { dg-warning "undefining '__cpp_nontype_template_args'" "" { target c++20 } }
#undef __cpp_nontype_template_parameter_auto // { dg-warning "undefining '__cpp_nontype_template_parameter_auto'" "" { target c++20 } }
#undef __cpp_nsdmi			// { dg-warning "undefining '__cpp_nsdmi'" "" { target c++20 } }
#undef __cpp_pack_indexing		// { dg-warning "undefining '__cpp_pack_indexing'" "" { target c++26 } }
#undef __cpp_placeholder_variables	// { dg-warning "undefining '__cpp_placeholder_variables'" "" { target c++26 } }
#undef __cpp_pp_embed			// { dg-warning "undefining '__cpp_pp_embed'" "" { target c++26 } }
#undef __cpp_range_based_for		// { dg-warning "undefining '__cpp_range_based_for'" "" { target c++20 } }
#undef __cpp_raw_strings		// { dg-warning "undefining '__cpp_raw_strings'" "" { target c++20 } }
#undef __cpp_ref_qualifiers		// { dg-warning "undefining '__cpp_ref_qualifiers'" "" { target c++20 } }
#undef __cpp_return_type_deduction	// { dg-warning "undefining '__cpp_return_type_deduction'" "" { target c++20 } }
#undef __cpp_rvalue_references		// { dg-warning "undefining '__cpp_rvalue_references'" "" { target c++20 } }
#undef __cpp_size_t_suffix		// { dg-warning "undefining '__cpp_size_t_suffix'" "" { target c++23 } }
#undef __cpp_sized_deallocation		// { dg-warning "undefining '__cpp_sized_deallocation'" "" { target c++20 } }
#undef __cpp_static_assert		// { dg-warning "undefining '__cpp_static_assert'" "" { target c++20 } }
#undef __cpp_static_call_operator	// { dg-warning "undefining '__cpp_static_call_operator'" "" { target c++23 } }
#undef __cpp_structured_bindings	// { dg-warning "undefining '__cpp_structured_bindings'" "" { target c++20 } }
#undef __cpp_template_parameters
#undef __cpp_template_template_args	// { dg-warning "undefining '__cpp_template_template_args'" "" { target c++20 } }
#undef __cpp_threadsafe_static_init	// { dg-warning "undefining '__cpp_threadsafe_static_init'" "" { target c++20 } }
#undef __cpp_trivial_union
#undef __cpp_unicode_characters		// { dg-warning "undefining '__cpp_unicode_characters'" "" { target c++20 } }
#undef __cpp_unicode_literals		// { dg-warning "undefining '__cpp_unicode_literals'" "" { target c++20 } }
#undef __cpp_user_defined_literals	// { dg-warning "undefining '__cpp_user_defined_literals'" "" { target c++20 } }
#undef __cpp_using_enum			// { dg-warning "undefining '__cpp_using_enum'" "" { target c++20 } }
#undef __cpp_variable_templates		// { dg-warning "undefining '__cpp_variable_templates'" "" { target c++20 } }
#undef __cpp_variadic_friend		// { dg-warning "undefining '__cpp_variadic_friend'" "" { target c++26 } }
#undef __cpp_variadic_templates		// { dg-warning "undefining '__cpp_variadic_templates'" "" { target c++20 } }
#undef __cpp_variadic_using		// { dg-warning "undefining '__cpp_variadic_using'" "" { target c++20 } }
