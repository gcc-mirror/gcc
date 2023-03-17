/* { dg-do compile } */
/* { dg-additional-options "-pedantic-errors" } */

/* When -pedantic-errors is passed, __has_extension should behave like
   __has_feature.  */

#if __has_feature (gnu_asm_goto_with_outputs)
#error extension recognized as feature
#endif

#if __has_extension (gnu_asm_goto_with_outputs)
#error pure extensions should not be recognized with -pedantic-errors
#endif

#if !__has_feature (tls) || !__has_extension (tls)
#error features should still be recognized with -pedantic-errors
#endif

/* Make this TU non-empty to appease -pedantic-errors.  */
int foo;
