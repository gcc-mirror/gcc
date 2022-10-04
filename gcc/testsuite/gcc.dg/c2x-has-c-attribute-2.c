/* Test __has_c_attribute.  Test supported attributes.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#if __has_c_attribute ( nodiscard ) != 202003L
#error "bad result for nodiscard"
#endif

#if __has_c_attribute ( __nodiscard__ ) != 202003L
#error "bad result for __nodiscard__"
#endif

#if __has_c_attribute(maybe_unused) != 202106L
#error "bad result for maybe_unused"
#endif

#if __has_c_attribute(__maybe_unused__) != 202106L
#error "bad result for __maybe_unused__"
#endif

#if __has_c_attribute (deprecated) != 201904L
#error "bad result for deprecated"
#endif

#if __has_c_attribute (__deprecated__) != 201904L
#error "bad result for __deprecated__"
#endif

#if __has_c_attribute (fallthrough) != 201910L
#error "bad result for fallthrough"
#endif

#if __has_c_attribute (__fallthrough__) != 201910L
#error "bad result for __fallthrough__"
#endif

#if __has_c_attribute (noreturn) != 202202L
#error "bad result for noreturn"
#endif

#if __has_c_attribute (__noreturn__) != 202202L
#error "bad result for __noreturn__"
#endif

#if __has_c_attribute (_Noreturn) != 202202L
#error "bad result for _Noreturn"
#endif

#if __has_c_attribute (___Noreturn__) != 202202L
#error "bad result for ___Noreturn__"
#endif
  
/* Macros in the attribute name are expanded.  */
#define foo deprecated
#if __has_c_attribute (foo) != 201904L
#error "bad result for foo"
#endif
