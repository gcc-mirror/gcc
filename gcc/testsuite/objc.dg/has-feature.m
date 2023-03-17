/* { dg-do compile } */

#define HAVE_C11 (__STDC_VERSION__ >= 201112L)

#if !__has_feature (objc_instancetype)
#error
#endif

#if !__has_feature (objc_default_synthesize_properties)
#error
#endif

/* C features should be available as extensions.  */
#if !__has_extension (c_alignas)
#error
#endif

/* And as features given the appropriate C standard.  */
#if __has_feature (c_alignas) != HAVE_C11
#error
#endif

/* Shouldn't have C++ features even as extensions.  */
#if __has_feature (cxx_constexpr) || __has_extension (cxx_constexpr)
#error
#endif
