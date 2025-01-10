/* { dg-do compile { target c++17 } } */
#include "allocate-allocator-handle.h"

/* Invalid allocator clause */

template<auto Alloc>
void auto_nttp_allocator()
{
  int a;
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" } */
}

template<auto Alloc>
void auto_nttp_allocator_uninstantiated()
{
  int a;
  #pragma omp allocate(a) allocator(Alloc)
}

#define DEFINITELY_NOT_PREDEFINED static_cast<omp_allocator_handle_t>(1024)

void instantiate_auto_nttp_allocator()
{
  auto_nttp_allocator<omp_default_mem_alloc>(); /* { dg-bogus "required from here" } */
  auto_nttp_allocator<DEFINITELY_NOT_PREDEFINED>(); /* { dg-bogus "required from here" } */
  auto_nttp_allocator<1>(); /* { dg-message "required from here" } */
}

#undef DEFINITELY_NOT_PREDEFINED

template<auto Alloc>
void auto_nttp_allocator_static_0()
{
  static int a;
  #pragma omp allocate(a) allocator(Alloc)
  /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" "" { target *-*-* } .-1 } */
  /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" "" { target *-*-* } .-2 } */
}

template<auto Alloc>
void auto_nttp_allocator_static_1()
{
  static int a;
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" } */
}

template<auto Alloc>
void auto_nttp_allocator_static_2()
{
  static int a;
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" } */
}

template<auto Alloc>
void auto_nttp_allocator_static_uninstantiated()
{
  static int a;
  #pragma omp allocate(a) allocator(Alloc)
}

#define DEFINITELY_NOT_PREDEFINED static_cast<omp_allocator_handle_t>(1024)

void instantiate_auto_nttp_allocator_static()
{
  auto_nttp_allocator_static_0<omp_default_mem_alloc>(); /* { dg-bogus "required from here" } */
  auto_nttp_allocator_static_0<1>(); /* { dg-message "required from here" } */
  auto_nttp_allocator_static_0<DEFINITELY_NOT_PREDEFINED>(); /* { dg-message "required from here" } */

  auto_nttp_allocator_static_1<omp_default_mem_alloc>(); /* { dg-bogus "required from here" } */
  auto_nttp_allocator_static_1<1>(); /* { dg-message "required from here" } */

  auto_nttp_allocator_static_2<omp_default_mem_alloc>(); /* { dg-bogus "required from here" } */
  auto_nttp_allocator_static_2<DEFINITELY_NOT_PREDEFINED>(); /* { dg-message "required from here" } */
}

#undef DEFINITELY_NOT_PREDEFINED

/* Invalid align clause */

template<auto Align>
void auto_nttp_align_uninstantiated()
{
  int a;
  #pragma omp allocate(a) align(Align)
}

template<auto Align>
void auto_nttp_align_0()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
}

template<auto Align>
void auto_nttp_align_1()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
}

void instantiate_auto_nttp_align()
{
  auto_nttp_align_0<32>(); /* { dg-bogus "required from here" } */
  auto_nttp_align_0<42>(); /* { dg-message "required from here" } */

  auto_nttp_align_1<32>(); /* { dg-bogus "required from here" } */
  auto_nttp_align_1<nullptr>(); /* { dg-message "required from here" } */
}
