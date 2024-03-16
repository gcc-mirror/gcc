/* { dg-do compile } */
/* { dg-options "-fno-openmp -fopenmp-simd" } */

#if !__has_attribute(omp::directive)
#error omp::directive not supported in C/C++
#endif

#if !__has_attribute(omp::sequence)
#error omp::sequence not supported in C/C++
#endif

#if !__has_attribute(omp::decl)
#error omp::decl not supported in C/C++
#endif

#if __has_attribute(omp::unknown)
#error omp::unknown supported
#endif

#if !__has_cpp_attribute(omp::directive)
#error omp::directive not supported in C/C++
#endif

#if !__has_cpp_attribute(omp::sequence)
#error omp::sequence not supported in C/C++
#endif

#if !__has_cpp_attribute(omp::decl)
#error omp::sequence not supported in C/C++
#endif

#if __has_cpp_attribute(omp::unknown)
#error omp::unknown supported
#endif

#if !__has_attribute(__omp__::__directive__)
#error __omp__::__directive__ not supported in C/C++
#endif

#if !__has_attribute(__omp__::__sequence__)
#error __omp__::__sequence__ not supported in C/C++
#endif

#if !__has_attribute(__omp__::__decl__)
#error __omp__::__decl__ not supported in C/C++
#endif

#if __has_attribute(__omp__::__unknown__)
#error __omp__::__unknown__ supported
#endif

#if !__has_cpp_attribute(__omp__::__directive__)
#error __omp__::__directive__ not supported in C/C++
#endif

#if !__has_cpp_attribute(__omp__::__sequence__)
#error __omp__::__sequence__ not supported in C/C++
#endif

#if !__has_cpp_attribute(__omp__::__decl__)
#error __omp__::__decl__ not supported in C/C++
#endif

#if __has_cpp_attribute(__omp__::__unknown__)
#error __omp__::__unknown__ supported
#endif

#if !__has_attribute(omp::__directive__)
#error omp::__directive__ not supported in C/C++
#endif

#if !__has_attribute(__omp__::sequence)
#error __omp__::sequence not supported in C/C++
#endif

#if !__has_attribute(omp::__decl__)
#error omp::__decl__ not supported in C/C++
#endif

#if __has_attribute(omp::__unknown__)
#error omp::__unknown__ supported
#endif

#if !__has_cpp_attribute(__omp__::directive)
#error __omp__::directive not supported in C/C++
#endif

#if !__has_cpp_attribute(omp::__sequence__)
#error omp::__sequence__ not supported in C/C++
#endif

#if !__has_cpp_attribute(__omp__::decl)
#error __omp__::decl not supported in C/C++
#endif

#if __has_cpp_attribute(__omp__::unknown)
#error __omp__::unknown supported
#endif
