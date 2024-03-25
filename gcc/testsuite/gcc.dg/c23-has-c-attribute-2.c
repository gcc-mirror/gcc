/* Test __has_c_attribute.  Test supported attributes.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#if __has_c_attribute ( nodiscard ) != 202311L
#error "bad result for nodiscard"
#endif

#if __has_c_attribute ( __nodiscard__ ) != 202311L
#error "bad result for __nodiscard__"
#endif

#if __has_c_attribute(maybe_unused) != 202311L
#error "bad result for maybe_unused"
#endif

#if __has_c_attribute(__maybe_unused__) != 202311L
#error "bad result for __maybe_unused__"
#endif

#if __has_c_attribute (deprecated) != 202311L
#error "bad result for deprecated"
#endif

#if __has_c_attribute (__deprecated__) != 202311L
#error "bad result for __deprecated__"
#endif

#if __has_c_attribute (fallthrough) != 202311L
#error "bad result for fallthrough"
#endif

#if __has_c_attribute (__fallthrough__) != 202311L
#error "bad result for __fallthrough__"
#endif

#if __has_c_attribute (noreturn) != 202311L
#error "bad result for noreturn"
#endif

#if __has_c_attribute (__noreturn__) != 202311L
#error "bad result for __noreturn__"
#endif

#if __has_c_attribute (_Noreturn) != 202311L
#error "bad result for _Noreturn"
#endif

#if __has_c_attribute (___Noreturn__) != 202311L
#error "bad result for ___Noreturn__"
#endif
  
/* Macros in the attribute name are expanded.  */
#define foo deprecated
#if __has_c_attribute (foo) != 202311L
#error "bad result for foo"
#endif
