/* { dg-do compile { target c++11 } } */
#include "allocate-allocator-handle.h"

/* Incorrect use of lambda captures in a directive or clause.
   There are a few cases in here that are impacted by the bug with implicit
   constexpr functions detailed in allocate-15.C and allocate-16.C.  */

void capture_used_in_directive()
{
  int a = 42;
  auto cl = [a](){
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  };
}

template<typename>
void capture_used_in_directive_templ_uninstantiated()
{
  int a = 42;
  auto cl = [a](){
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  };
}

template<typename>
void capture_used_in_directive_templ()
{
  int a = 42;
  auto cl = [a](){
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  };
}

void instantiate_capture_used_in_directive()
{
  capture_used_in_directive_templ<void>();
}



void capture_used_in_allocator_clause_static_var()
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  auto cl = [alloc](){
    static int a = 42;
    #pragma omp allocate(a) allocator(alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" "" { xfail c++17 } } */
    /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" "" { target c++17 } .-1 } */
  };
}

/* This is similar to capture_used_in_align_clause_templ_uninstantiated below,
   see below for more info.  */
template<typename>
void capture_used_in_allocator_clause_static_var_templ_uninstantiated()
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  auto cl = [alloc](){
    static int a = 42;
    #pragma omp allocate(a) allocator(alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" "" { xfail *-*-* } } */
    /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" "" { target c++17 } .-1 } */
  };
}

/* It's not viable to diagnose this case, see the comment on
   dependent_capture_used_in_align_clause_templ_uninstantiated below.  */
template<typename T>
void dependent_capture_used_in_allocator_clause_static_var_templ_uninstantiated()
{
  T alloc = omp_default_mem_alloc;
  auto cl = [alloc](){
    static int a = 42;
    #pragma omp allocate(a) allocator(alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" "" { xfail *-*-* } } */
    /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" "" { target c++17 } .-1 } */
  };
}

template<typename>
void capture_used_in_allocator_clause_static_var_templ()
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  auto cl = [alloc](){
    static int a = 42;
    #pragma omp allocate(a) allocator(alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" "" { xfail c++17 } } */
    /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" "" { target c++17 } .-1 } */
  };
}

template<typename T>
void dependent_capture_used_in_allocator_clause_static_var_templ()
{
  T alloc = omp_default_mem_alloc;
  auto cl = [alloc](){
    static int a = 42;
    #pragma omp allocate(a) allocator(alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" "" { xfail c++17 } } */
    /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" "" { target c++17 } .-1 } */
  };
}

void instantiate_capture_used_in_allocator_clause_static_var()
{
  capture_used_in_allocator_clause_static_var_templ<void>();
  dependent_capture_used_in_allocator_clause_static_var_templ<omp_allocator_handle_t>();
}



void capture_used_in_align_clause()
{
  int align = 32;
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
  };
}

/* This case should be diagnosable, but we don't even try right now.  Even if
   we did try by checking if an expr is potential_constant_expression, it
   appears to incorrectly returns true for the align clause's expression here.
   We know the type is int so we know it's not an empty type, so we should be
   able to know that it can't possibly be a constant expression even before
   the template is instantiated.  Unfortunately, as stated above,
   potential_constant_expression does not agree.  It's also possible this is
   not a bug in potential_constant_expression and there is something I am
   overlooking.  */
template<typename>
void capture_used_in_align_clause_templ_uninstantiated()
{
  int align = 32;
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
  };
}

/* Unlike the above case this case is very hard to diagnose,
   potential_constant_expression correctly returns true for the align clause's
   expr here.  See cp/semantics.cc:finish_omp_allocate for more information.  */
template<typename T>
void dependent_capture_used_in_align_clause_templ_uninstantiated()
{
  T align = 32;
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align) /* { eg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
  };
}

template<typename>
void capture_used_in_align_clause_templ()
{
  int align = 32;
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
  };
}

template<typename T>
void dependent_capture_used_in_align_clause_templ()
{
  T align = 32;
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
  };
}

void instantiate_capture_used_in_align()
{
  capture_used_in_align_clause_templ<void>();
  dependent_capture_used_in_align_clause_templ<int>();
}
