/* { dg-do compile } */
/* { dg-options "-fno-openmp -fno-openmp-simd" } */

#if __has_attribute(omp::directive)
#error omp::directive supported even when -fno-openmp{,-simd}
#endif

#if __has_attribute(omp::sequence)
#error omp::sequence supported even when -fno-openmp{,-simd}
#endif

#if __has_attribute(omp::unknown)
#error omp::unknown supported
#endif

#if __has_cpp_attribute(omp::directive)
#error omp::directive supported even when -fno-openmp{,-simd}
#endif

#if __has_cpp_attribute(omp::sequence)
#error omp::sequence supported even when -fno-openmp{,-simd}
#endif

#if __has_cpp_attribute(omp::unknown)
#error omp::unknown supported
#endif

#if __has_attribute(__omp__::__directive__)
#error __omp__::__directive__ supported even when -fno-openmp{,-simd}
#endif

#if __has_attribute(__omp__::__sequence__)
#error __omp__::__sequence__ supported even when -fno-openmp{,-simd}
#endif

#if __has_attribute(__omp__::__unknown__)
#error __omp__::__unknown__ supported
#endif

#if __has_cpp_attribute(__omp__::__directive__)
#error __omp__::__directive__ supported even when -fno-openmp{,-simd}
#endif

#if __has_cpp_attribute(__omp__::__sequence__)
#error __omp__::__sequence__ supported even when -fno-openmp{,-simd}
#endif

#if __has_cpp_attribute(__omp__::__unknown__)
#error __omp__::__unknown__ supported
#endif

#if __has_attribute(omp::__directive__)
#error omp::__directive__ supported even when -fno-openmp{,-simd}
#endif

#if __has_attribute(__omp__::sequence)
#error __omp__::sequence supported even when -fno-openmp{,-simd}
#endif

#if __has_attribute(omp::__unknown__)
#error omp::__unknown__ supported
#endif

#if __has_cpp_attribute(__omp__::directive)
#error __omp__::directive supported even when -fno-openmp{,-simd}
#endif

#if __has_cpp_attribute(omp::__sequence__)
#error omp::__sequence__ supported even when -fno-openmp{,-simd}
#endif

#if __has_cpp_attribute(__omp__::unknown)
#error __omp__::unknown supported
#endif
