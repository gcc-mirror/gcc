/* PR c/66970 - Add __has_builtin() macro
   Verify __has_builtin evaluation for disabled library built-ins.
   { dg-do compile }
   { dg-options "-fno-builtin-abs" }
   { dg-additional-options "-std=c90" { target c } } */

#if !__has_builtin (__builtin_abs)
// __builtin_xxx is always available regardless of -fno-builtin.
#  error "__has_builtin (__builtin_abs) failed"
#endif

#if __has_builtin (abs)
#  error "__has_builtin (abs) failed"
#endif

#if __has_builtin (abs)
#  error "__has_builtin (abs) failed"
#endif


#if !__has_builtin (__builtin_vsnprintf)
// __builtin_vsnprintf is available in all language modes.
#  error "__has_builtin (__builtin_vsnprintf) failed"
#endif

#if !__has_builtin (vsnprintf)
#  if __cplusplus
// vsnprintf is always available in C++.
#    error "__has_builtin (vsnprintf) failed"
#  endif
#else
#  if !__cplusplus
// vsnprintf is a C99 function not available in C90.
#    error "__has_builtin (vsnprintf) failed"
#  endif
#endif
