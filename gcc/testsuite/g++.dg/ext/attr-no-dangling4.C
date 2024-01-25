// { dg-do compile { target c++11 } }

#if !__has_attribute(no_dangling)
#error unsupported
#endif

#ifdef __has_cpp_attribute
# if !__has_cpp_attribute(no_dangling)
#  error no_dangling
# endif
#endif

struct [[gnu::no_dangling]] S { };
static_assert (__builtin_has_attribute (S, no_dangling), "");
