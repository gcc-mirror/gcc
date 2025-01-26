/* { dg-do preprocess } */

#if __has_builtin (__builtin_va_start) != 1
#error "No __builtin_va_start"
#endif
#if __has_builtin (__builtin_va_end) != 1
#error "No __builtin_va_end"
#endif
#if __has_builtin (__builtin_va_arg) != 1
#error "no __builtin_va_arg"
#endif
#if __STDC_VERSION__ >= 202311L
#if __has_builtin (__builtin_c23_va_start) != 1
#error "no __builtin_c23_va_start"
#endif
#endif
