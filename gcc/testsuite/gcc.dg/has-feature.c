/* { dg-do compile } */
/* { dg-options "" } */
/* Test __has_{feature,extension} for C language features.  */

#if !__has_extension (c_alignas) || !__has_extension (c_alignof)
#error
#endif

#if !__has_extension (c_atomic) || !__has_extension (c_generic_selections)
#error
#endif

#if !__has_extension (c_static_assert) || !__has_extension (c_thread_local)
#error
#endif

#if !__has_extension (cxx_binary_literals)
#error
#endif

#if  __STDC_VERSION__ >= 201112L
/* Have C11 features.  */
#if !__has_feature (c_alignas) || !__has_feature (c_alignof)
#error
#endif

#if !__has_feature (c_atomic) || !__has_feature (c_generic_selections)
#error
#endif

#if !__has_feature (c_static_assert) || !__has_feature (c_thread_local)
#error
#endif

#else
/* Don't have C11 features.  */
#if __has_feature (c_alignas) || __has_feature (c_alignof)
#error
#endif

#if __has_feature (c_atomic) || __has_feature (c_generic_selections)
#error
#endif

#if __has_feature (c_static_assert) || __has_feature (c_thread_local)
#error
#endif

#endif

#if __STDC_VERSION__ >= 202000L
/* Have C2x features.  */
#if !__has_feature (cxx_binary_literals)
#error
#endif

#else
/* Don't have C2x features.  */
#if __has_feature (cxx_binary_literals)
#error
#endif
#endif
