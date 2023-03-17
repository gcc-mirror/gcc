/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */
#define FEAT(x) (__has_feature (x) && __has_extension (x))
#if !FEAT (undefined_behavior_sanitizer)
#error
#endif
