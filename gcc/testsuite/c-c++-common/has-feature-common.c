/* { dg-do compile } */
/* Test __has_{feature,extension} for generic features.  */

#define FEAT(x) (__has_feature (x) && __has_extension (x))
#define EXT(x) (__has_extension (x) && !__has_feature (x))

#if __has_feature (unknown_feature) || __has_extension (unknown_feature)
#error unknown feature is known!
#endif

#if !EXT (gnu_asm_goto_with_outputs)
#error
#endif

#if !EXT (__gnu_asm_goto_with_outputs__)
#error
#endif

#if !EXT (gnu_asm_goto_with_outputs_full)
#error
#endif

#if !EXT (__gnu_asm_goto_with_outputs_full__)
#error
#endif

#if !FEAT (enumerator_attributes)
#error
#endif

#if !FEAT (__enumerator_attributes__)
#error
#endif

#if !FEAT (attribute_deprecated_with_message)
#error
#endif

#if !FEAT (__attribute_deprecated_with_message__)
#error
#endif

#if !FEAT (attribute_unavailable_with_message)
#error
#endif

#if !FEAT (__attribute_unavailable_with_message__)
#error
#endif

#if !FEAT (tls)
#error
#endif

#if !FEAT(__tls__)
#error
#endif

#if defined (__SANITIZE_ADDRESS__) != __has_feature (address_sanitizer)
#error
#endif

#if defined (__SANITIZE_ADDRESS__) != __has_extension (address_sanitizer)
#error
#endif

#if defined (__SANITIZE_THREAD__) != __has_feature (thread_sanitizer)
#error
#endif

#if defined (__SANITIZE_THREAD__) != __has_extension (thread_sanitizer)
#error
#endif
