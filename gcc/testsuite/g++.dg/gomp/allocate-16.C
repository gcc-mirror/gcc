/* { dg-do compile { target c++14 } } */
/* { dg-additional-options "-fimplicit-constexpr" } */
#include "allocate-allocator-handle.h"

/* Check application of align clause to static variables used in an OpenMP
   allocate directive in functions that are implicitly constexpr.
   Inline functions and function templates with -fimplicit-constexpr.

   See allocate-15.C for more information, and cases using lambdas.
   
   For now, we simply do not support these cases.  */

/* Making a regex for demangled identifiers is actually way harder than making
   a regex for mangled ones, too many escapes are needed.

   We need to ODR-use the regular functions to force them to be emitted.  */

/* { dg-final { scan-assembler "\.align 256\\s*\.type\\s*_ZZ6f0_256vE1a" } } */
inline int* f0_256()
{
  static int a = 42;
  #pragma omp allocate(a) align(256) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
  return &a;
}
constexpr int*(*odr_use_f0_256)() = &f0_256;

/* { dg-final { scan-assembler "\.align 512\\s*\.type\\s*_ZZ6f0_512vE1a" } } */
inline int* f0_512()
{
  static int a = 42;
  #pragma omp allocate(a) align(512) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
  return &a;
}
constexpr int*(*odr_use_f0_512)() = &f0_512;

/* { dg-final { scan-assembler "\.align 1024\\s*\.type\\s*_ZZ7f0_1024vE1a" } } */
inline int* f0_1024()
{
  static int a = 42;
  #pragma omp allocate(a) align(1024) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
  return &a;
}
constexpr int*(*odr_use_f0_1024)() = &f0_1024;



/* { dg-final { scan-assembler "\.align 256\\s*\.type\\s*_ZZ6f1_256IvEPivE1a" } } */
template<typename>
inline int* f1_256()
{
  static int a = 42;
  #pragma omp allocate(a) align(256) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
  return &a;
}
template int* f1_256<void>();

/* { dg-final { scan-assembler "\.align 512\\s*\.type\\s*_ZZ6f1_512IvEPivE1a" } } */
template<typename>
inline int* f1_512()
{
  static int a = 42;
  #pragma omp allocate(a) align(512) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
  return &a;
}
template int* f1_512<void>();

/* { dg-final { scan-assembler "\.align 1024\\s*\.type\\s*_ZZ7f1_1024IvEPivE1a" } } */
template<typename>
inline int* f1_1024()
{
  static int a = 42;
  #pragma omp allocate(a) align(1024) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
  return &a;
}
template int* f1_1024<void>();
