// { dg-do compile }
// { dg-options "" }

// PR c++/113658: we shouldn't declare support for
// cxx_constexpr_string_builtins as GCC is missing some of the builtins
// that clang implements.

#if __has_feature (cxx_constexpr_string_builtins)
#error
#endif

#if __has_extension (cxx_constexpr_string_builtins)
#error
#endif
