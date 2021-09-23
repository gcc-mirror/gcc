/* { dg-do compile } */
/* { dg-options "-fno-openmp -fopenmp-simd" } */

#if __has_attribute(omp::directive)
#ifndef __cplusplus
#error omp::directive supported in C
#endif
#else
#ifdef __cplusplus
#error omp::directive not supported in C++
#endif
#endif

#if __has_attribute(omp::sequence)
#ifndef __cplusplus
#error omp::sequence supported in C
#endif
#else
#ifdef __cplusplus
#error omp::sequence not supported in C++
#endif
#endif

#if __has_attribute(omp::unknown)
#error omp::unknown supported
#endif

#if __has_cpp_attribute(omp::directive)
#ifndef __cplusplus
#error omp::directive supported in C
#endif
#else
#ifdef __cplusplus
#error omp::directive not supported in C++
#endif
#endif

#if __has_cpp_attribute(omp::sequence)
#ifndef __cplusplus
#error omp::sequence supported in C
#endif
#else
#ifdef __cplusplus
#error omp::sequence not supported in C++
#endif
#endif

#if __has_cpp_attribute(omp::unknown)
#error omp::unknown supported
#endif

#if __has_attribute(__omp__::__directive__)
#ifndef __cplusplus
#error __omp__::__directive__ supported in C
#endif
#else
#ifdef __cplusplus
#error __omp__::__directive__ not supported in C++
#endif
#endif

#if __has_attribute(__omp__::__sequence__)
#ifndef __cplusplus
#error __omp__::__sequence__ supported in C
#endif
#else
#ifdef __cplusplus
#error __omp__::__sequence__ not supported in C++
#endif
#endif

#if __has_attribute(__omp__::__unknown__)
#error __omp__::__unknown__ supported
#endif

#if __has_cpp_attribute(__omp__::__directive__)
#ifndef __cplusplus
#error __omp__::__directive__ supported in C
#endif
#else
#ifdef __cplusplus
#error __omp__::__directive__ not supported in C++
#endif
#endif

#if __has_cpp_attribute(__omp__::__sequence__)
#ifndef __cplusplus
#error __omp__::__sequence__ supported in C
#endif
#else
#ifdef __cplusplus
#error __omp__::__sequence__ not supported in C++
#endif
#endif

#if __has_cpp_attribute(__omp__::__unknown__)
#error __omp__::__unknown__ supported
#endif

#if __has_attribute(omp::__directive__)
#ifndef __cplusplus
#error omp::__directive__ supported in C
#endif
#else
#ifdef __cplusplus
#error omp::__directive__ not supported in C++
#endif
#endif

#if __has_attribute(__omp__::sequence)
#ifndef __cplusplus
#error __omp__::sequence supported in C
#endif
#else
#ifdef __cplusplus
#error __omp__::sequence not supported in C++
#endif
#endif

#if __has_attribute(omp::__unknown__)
#error omp::__unknown__ supported
#endif

#if __has_cpp_attribute(__omp__::directive)
#ifndef __cplusplus
#error __omp__::directive supported in C
#endif
#else
#ifdef __cplusplus
#error __omp__::directive not supported in C++
#endif
#endif

#if __has_cpp_attribute(omp::__sequence__)
#ifndef __cplusplus
#error omp::__sequence__ supported in C
#endif
#else
#ifdef __cplusplus
#error omp::__sequence__ not supported in C++
#endif
#endif

#if __has_cpp_attribute(__omp__::unknown)
#error __omp__::unknown supported
#endif
