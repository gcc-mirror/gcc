/* { dg-do compile } */
/* { dg-options "" } */

#if !__has_attribute (musttail)
#error missing musttail attribute
#endif
#ifdef __cplusplus
#if !__has_cpp_attribute (gnu::musttail)
#error missing gnu::musttail attribute
#endif
#if !__has_cpp_attribute (clang::musttail)
#error missing clang::musttail attribute
#endif
#else
#if !__has_c_attribute (gnu::musttail)
#error missing gnu::musttail attribute
#endif
#if !__has_c_attribute (clang::musttail)
#error missing clang::musttail attribute
#endif
#endif
