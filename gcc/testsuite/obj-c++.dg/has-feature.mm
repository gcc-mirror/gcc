// { dg-do compile }

#define CXX11 (__cplusplus >= 201103L)

#if !__has_feature (objc_instancetype)
#error
#endif

#if !__has_feature (objc_default_synthesize_properties)
#error
#endif

// C features should not be available.
#if __has_extension (c_alignas) || __has_feature (c_alignof)
#error
#endif

// C++ features should be available (given the right standard).
#if __has_feature (cxx_constexpr) != CXX11
#error
#endif
