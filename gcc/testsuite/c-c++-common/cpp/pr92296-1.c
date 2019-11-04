/* PR preprocessor/92296 */
/* { dg-do preprocess } */

#pragma push_macro("__TIMESTAMP__")
#pragma pop_macro("__TIMESTAMP__")

#pragma push_macro("__TIME__")
#pragma pop_macro("__TIME__")

#pragma push_macro("__DATE__")
#pragma pop_macro("__DATE__")

#pragma push_macro("__FILE__")
#pragma pop_macro("__FILE__")

#pragma push_macro("__BASE_FILE__")
#pragma pop_macro("__BASE_FILE__")

#pragma push_macro("__LINE__")
#pragma pop_macro("__LINE__")

#pragma push_macro("__INCLUDE_LEVEL__")
#pragma pop_macro("__INCLUDE_LEVEL__")

#pragma push_macro("__COUNTER__")
#pragma pop_macro("__COUNTER__")

#pragma push_macro("__has_attribute")
#pragma pop_macro("__has_attribute")

#pragma push_macro("__has_cpp_attribute")
#pragma pop_macro("__has_cpp_attribute")

#pragma push_macro("__has_builtin")
#pragma pop_macro("__has_builtin")
