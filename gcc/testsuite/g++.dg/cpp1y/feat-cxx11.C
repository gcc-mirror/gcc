// { dg-do compile }
// { dg-options "-std=gnu++11" }

#ifndef __cpp_unicode_characters
#  error "__cpp_unicode_characters"
#elif __cpp_unicode_characters != 200704
#  error "__cpp_unicode_characters != 200704"
#endif

#ifndef __cpp_raw_strings
#  error "__cpp_raw_strings"
#elif __cpp_raw_strings != 200710
#  error "__cpp_raw_strings != 200710"
#endif

#ifndef __cpp_unicode_literals
#  error "__cpp_unicode_literals"
#elif __cpp_unicode_literals != 200710
#  error "__cpp_unicode_literals != 200710"
#endif

#ifndef __cpp_user_defined_literals
#  error "__cpp_user_defined_literals"
#elif __cpp_user_defined_literals != 200809
#  error "__cpp_user_defined_literals != 200809"
#endif

#ifndef __cpp_lambdas
#  error "__cpp_lambdas"
#elif __cpp_lambdas != 200907
#  error "__cpp_lambdas != 200907"
#endif

#ifndef __cpp_constexpr
#  error "__cpp_constexpr"
#elif __cpp_constexpr != 200704
#  error "__cpp_constexpr != 200704"
#endif

#ifndef __cpp_static_assert
#  error "__cpp_static_assert"
#elif __cpp_static_assert != 200410
#  error "__cpp_static_assert != 200410"
#endif

#ifndef __cpp_decltype
#  error "__cpp_decltype"
#elif __cpp_decltype != 200707
#  error "__cpp_decltype != 200707"
#endif

#ifndef __cpp_attributes
#  error "__cpp_attributes"
#elif __cpp_attributes != 200809
#  error "__cpp_attributes != 200809"
#endif

#ifndef __cpp_rvalue_reference
#  error "__cpp_rvalue_reference"
#elif __cpp_rvalue_reference != 200610
#  error "__cpp_rvalue_reference != 200610"
#endif

#ifndef __cpp_variadic_templates
#  error "__cpp_variadic_templates"
#elif __cpp_variadic_templates != 200704
#  error "__cpp_variadic_templates != 200704"
#endif

#ifndef __cpp_alias_templates
#  error "__cpp_alias_templates"
#elif __cpp_alias_templates != 200704
#  error "__cpp_alias_templates != 200704"
#endif

//  These C++14 features are allowed in C++11 in non-ANSI modes.
#ifndef __cpp_binary_literals
#  error "__cpp_binary_literals"
#elif __cpp_binary_literals != 201304
#  error "__cpp_binary_literals != 201304"
#endif
