// PR c++/106759
// { dg-do compile }
// Verify that __has_builtin gives the correct answer for C++ built-ins.

#if !__has_builtin (__builtin_addressof)
# error "__has_builtin (__builtin_addressof) failed"
#endif
#if !__has_builtin (__builtin_bit_cast)
# error "__has_builtin (__builtin_bit_cast) failed"
#endif
#if !__has_builtin (__builtin_is_constant_evaluated)
# error "__has_builtin (__builtin_is_constant_evaluated) failed"
#endif
#if !__has_builtin (__builtin_is_corresponding_member)
# error "__has_builtin (__builtin_is_corresponding_member) failed"
#endif
#if !__has_builtin (__builtin_is_pointer_interconvertible_with_class)
# error "__has_builtin (__builtin_is_pointer_interconvertible_with_class) failed"
#endif
#if !__has_builtin (__builtin_launder)
# error "__has_builtin (__builtin_launder) failed"
#endif
#if !__has_builtin (__builtin_source_location)
# error "__has_builtin (__builtin_source_location) failed"
#endif
#if !__has_builtin (__has_nothrow_assign)
# error "__has_builtin (__has_nothrow_assign) failed"
#endif
#if !__has_builtin (__has_nothrow_constructor)
# error "__has_builtin (__has_nothrow_constructor) failed"
#endif
#if !__has_builtin (__has_nothrow_copy)
# error "__has_builtin (__has_nothrow_copy) failed"
#endif
#if !__has_builtin (__has_trivial_assign)
# error "__has_builtin (__has_trivial_assign) failed"
#endif
#if !__has_builtin (__has_trivial_constructor)
# error "__has_builtin (__has_trivial_constructor) failed"
#endif
#if !__has_builtin (__has_trivial_copy)
# error "__has_builtin (__has_trivial_copy) failed"
#endif
#if !__has_builtin (__has_trivial_destructor)
# error "__has_builtin (__has_trivial_destructor) failed"
#endif
#if !__has_builtin (__has_unique_object_representations)
# error "__has_builtin (__has_unique_object_representations) failed"
#endif
#if !__has_builtin (__has_virtual_destructor)
# error "__has_builtin (__has_virtual_destructor) failed"
#endif
#if !__has_builtin (__is_abstract)
# error "__has_builtin (__is_abstract) failed"
#endif
#if !__has_builtin (__is_aggregate)
# error "__has_builtin (__is_aggregate) failed"
#endif
#if !__has_builtin (__is_array)
# error "__has_builtin (__is_array) failed"
#endif
#if !__has_builtin (__is_assignable)
# error "__has_builtin (__is_assignable) failed"
#endif
#if !__has_builtin (__is_base_of)
# error "__has_builtin (__is_base_of) failed"
#endif
#if !__has_builtin (__is_bounded_array)
# error "__has_builtin (__is_bounded_array) failed"
#endif
#if !__has_builtin (__is_class)
# error "__has_builtin (__is_class) failed"
#endif
#if !__has_builtin (__is_constructible)
# error "__has_builtin (__is_constructible) failed"
#endif
#if !__has_builtin (__is_convertible)
# error "__has_builtin (__is_convertible) failed"
#endif
#if !__has_builtin (__is_empty)
# error "__has_builtin (__is_empty) failed"
#endif
#if !__has_builtin (__is_enum)
# error "__has_builtin (__is_enum) failed"
#endif
#if !__has_builtin (__is_final)
# error "__has_builtin (__is_final) failed"
#endif
#if !__has_builtin (__is_function)
# error "__has_builtin (__is_function) failed"
#endif
#if !__has_builtin (__is_layout_compatible)
# error "__has_builtin (__is_layout_compatible) failed"
#endif
#if !__has_builtin (__is_literal_type)
# error "__has_builtin (__is_literal_type) failed"
#endif
#if !__has_builtin (__is_member_function_pointer)
# error "__has_builtin (__is_member_function_pointer) failed"
#endif
#if !__has_builtin (__is_member_object_pointer)
# error "__has_builtin (__is_member_object_pointer) failed"
#endif
#if !__has_builtin (__is_member_pointer)
# error "__has_builtin (__is_member_pointer) failed"
#endif
#if !__has_builtin (__is_nothrow_assignable)
# error "__has_builtin (__is_nothrow_assignable) failed"
#endif
#if !__has_builtin (__is_nothrow_constructible)
# error "__has_builtin (__is_nothrow_constructible) failed"
#endif
#if !__has_builtin (__is_nothrow_convertible)
# error "__has_builtin (__is_nothrow_convertible) failed"
#endif
#if !__has_builtin (__is_object)
# error "__has_builtin (__is_object) failed"
#endif
#if !__has_builtin (__is_pointer_interconvertible_base_of)
# error "__has_builtin (__is_pointer_interconvertible_base_of) failed"
#endif
#if !__has_builtin (__is_pod)
# error "__has_builtin (__is_pod) failed"
#endif
#if !__has_builtin (__is_polymorphic)
# error "__has_builtin (__is_polymorphic) failed"
#endif
#if !__has_builtin (__is_reference)
# error "__has_builtin (__is_reference) failed"
#endif
#if !__has_builtin (__is_same)
# error "__has_builtin (__is_same) failed"
#endif
#if !__has_builtin (__is_same_as)
# error "__has_builtin (__is_same_as) failed"
#endif
#if !__has_builtin (__is_scoped_enum)
# error "__has_builtin (__is_scoped_enum) failed"
#endif
#if !__has_builtin (__is_standard_layout)
# error "__has_builtin (__is_standard_layout) failed"
#endif
#if !__has_builtin (__is_trivial)
# error "__has_builtin (__is_trivial) failed"
#endif
#if !__has_builtin (__is_trivially_assignable)
# error "__has_builtin (__is_trivially_assignable) failed"
#endif
#if !__has_builtin (__is_trivially_constructible)
# error "__has_builtin (__is_trivially_constructible) failed"
#endif
#if !__has_builtin (__is_trivially_copyable)
# error "__has_builtin (__is_trivially_copyable) failed"
#endif
#if !__has_builtin (__is_union)
# error "__has_builtin (__is_union) failed"
#endif
#if !__has_builtin (__reference_constructs_from_temporary)
# error "__has_builtin (__reference_constructs_from_temporary) failed"
#endif
#if !__has_builtin (__reference_converts_from_temporary)
# error "__has_builtin (__reference_converts_from_temporary) failed"
#endif
#if !__has_builtin (__remove_cv)
# error "__has_builtin (__remove_cv) failed"
#endif
#if !__has_builtin (__remove_cvref)
# error "__has_builtin (__remove_cvref) failed"
#endif
#if !__has_builtin (__remove_pointer)
# error "__has_builtin (__remove_pointer) failed"
#endif
#if !__has_builtin (__remove_reference)
# error "__has_builtin (__remove_reference) failed"
#endif
#if !__has_builtin (__underlying_type)
# error "__has_builtin (__underlying_type) failed"
#endif
