/* { dg-do compile } */
/* { dg-options "-fsanitize=address" } */
#define FEAT(x) (__has_feature (x) && __has_extension (x))
#if !FEAT (address_sanitizer)
#error
#endif
