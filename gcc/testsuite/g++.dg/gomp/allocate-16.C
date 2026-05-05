/* { dg-do compile { target c++11 } } */
/* { dg-ice "" { c++17 } } */
#include "allocate-allocator-handle.h"

/* Incorrect use of lambda captures in a directive or clause.
   There are a few cases in here that are impacted by the bug with implicit
   constexpr functions.  */

/* These errors (specifically in capture_used_in_directive) really could be better.  */

void capture_used_in_directive()
{
  int a = 42;
  auto cl = [a](){ /* { dg-note "declared here" } */
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  };
}

template<typename>
void capture_used_in_directive_templ_uninstantiated()
{
  int a = 42;
  auto cl = [a](){ /* { dg-note "declared here" } */
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  };
}

template<typename>
void capture_used_in_directive_templ()
{
  int a = 42;
  auto cl = [a](){ /* { dg-note "declared here" } */
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  };
}

void instantiate_capture_used_in_directive()
{
  capture_used_in_directive_templ<void>();
}



void capture_used_in_allocator_clause_static_var()
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc; /* { dg-note "'omp_allocator_handle_t alloc' is not const" } */
  auto cl = [alloc](){
    static int a = 42; /* { dg-note "'a' declared here" } */
    int b = 42;	       /* { dg-bogus "'b' declared here" } */
    static int c = 42; /* { dg-note "'c' declared here" } */
    int d = 42;        /* { dg-bogus "'d' declared here" } */
    #pragma omp allocate(a, b, c, d) allocator(alloc)
    /* { dg-error "the value of 'alloc' is not usable in a constant expression" "" { target *-*-* } .-1 } */
    /* { dg-error "'allocator' clause requires a constant predefined allocator" "" { target *-*-* } .-2 } */
    /* { dg-note "because one or more variables with static storage duration appear in the 'allocate' directive" "" { target *-*-* } .-3 } */
  };
}

template<typename>
void capture_used_in_allocator_clause_static_var_templ_uninstantiated()
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc; /* { dg-note "'omp_allocator_handle_t alloc' is not const" "" { xfail *-*-* } } */
  auto cl = [alloc](){
    static int a = 42; /* { dg-note "'a' declared here" "" { xfail *-*-* } } */
    int b = 42;	       /* { dg-bogus "'b' declared here" } */
    static int c = 42; /* { dg-note "'c' declared here" "" { xfail *-*-* } } */
    int d = 42;        /* { dg-bogus "'d' declared here" } */
    #pragma omp allocate(a, b, c, d) allocator(alloc)
    /* { dg-error "the value of 'alloc' is not usable in a constant expression" "" { xfail *-*-* } .-1 } */
    /* { dg-error "'allocator' clause requires a constant predefined allocator" "" { xfail *-*-* } .-2 } */
    /* { dg-note "because one or more variables with static storage duration appear in the 'allocate' directive" "" { xfail *-*-* } .-3 } */
  };
}
/* This case can't be diagnosed, there exists a T where alloc is a converted
   constant expression of type omp_allocator_handle_t.  This is demonstrated
   below in dependent_capture_used_in_allocator_clause_static_var_templ_valid.  */
template<typename T>
void dependent_capture_used_in_allocator_clause_static_var_templ_uninstantiated()
{
  T alloc = omp_default_mem_alloc; /* { dg-bogus "" } */
  auto cl = [alloc](){
    static int a = 42; /* { dg-bogus "'a' declared here" } */
    static int c = 42; /* { dg-bogus "'c' declared here" } */
    #pragma omp allocate(a, c) allocator(alloc)
    /* { dg-bogus "the value of 'alloc' is not usable in a constant expression" "" { target *-*-* } .-1 } */
    /* { dg-bogus "because one or more variables with static storage duration appear in the 'allocate' directive" "" { target *-*-* } .-2 } */
  };
}

template<typename>
void capture_used_in_allocator_clause_static_var_templ()
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc; /* { dg-note "'omp_allocator_handle_t alloc' is not const" "" { xfail c++17 } } */
  auto cl = [alloc](){
    static int a = 42; /* { dg-note "'a' declared here" "" { xfail c++17 } } */
    int b = 42;	       /* { dg-bogus "'b' declared here" } */
    static int c = 42; /* { dg-note "'c' declared here" "" { xfail c++17 } } */
    int d = 42;        /* { dg-bogus "'d' declared here" } */
    #pragma omp allocate(a, b, c, d) allocator(alloc)
    /* { dg-error "the value of 'alloc' is not usable in a constant expression" "" { xfail c++17 } .-1 }*/
    /* { dg-error "'allocator' clause requires a constant predefined allocator" "" { xfail c++17 } .-2 } */
    /* { dg-note "because one or more variables with static storage duration appear in the 'allocate' directive" "" { xfail c++17 } .-3 } */
  };
}

template<typename T>
void dependent_capture_used_in_allocator_clause_static_var_templ()
{
  T alloc = omp_default_mem_alloc; /* { dg-note "'omp_allocator_handle_t alloc' is not const" "" { xfail c++17 } } */
  auto cl = [alloc](){
    static int a = 42; /* { dg-note "'a' declared here" "" { xfail c++17 } } */
    int b = 42;	       /* { dg-bogus "'b' declared here" } */
    static int c = 42; /* { dg-note "'c' declared here" "" { xfail c++17 } } */
    int d = 42;        /* { dg-bogus "'d' declared here" } */
    #pragma omp allocate(a, b, c, d) allocator(alloc)
    /* { dg-error "the value of 'alloc' is not usable in a constant expression" "" { xfail c++17 } .-1 } */
    /* { dg-error "'allocator' clause requires a constant predefined allocator" "" { xfail c++17 } .-2 } */
    /* { dg-note "because one or more variables with static storage duration appear in the 'allocate' directive" "" { xfail c++17 } .-3 } */
  };
}

template<typename T>
void dependent_capture_used_in_allocator_clause_static_var_templ_valid()
{
  T alloc = omp_default_mem_alloc; /* { dg-bogus "" } */
  auto cl = [alloc](){
    static int a = 42; /* { dg-bogus "" } */
    static int c = 42; /* { dg-bogus "" } */
    #pragma omp allocate(a, c) allocator(alloc)
    /* { dg-bogus "the value of 'alloc' is not usable in a constant expression" "" { target *-*-* } .-1 } */
    /* { dg-bogus "'allocator' clause requires a constant predefined allocator" "" { target *-*-* } .-2 } */
  };
}

void instantiate_capture_used_in_allocator_clause_static_var()
{
  capture_used_in_allocator_clause_static_var_templ<void>();
  dependent_capture_used_in_allocator_clause_static_var_templ<omp_allocator_handle_t>();

  struct S {
    constexpr S (omp_allocator_handle_t) {}
    constexpr operator omp_allocator_handle_t () const { return omp_default_mem_alloc; }
  };
  dependent_capture_used_in_allocator_clause_static_var_templ_valid<S> ();
  dependent_capture_used_in_allocator_clause_static_var_templ_valid<const omp_allocator_handle_t> ();
}



void capture_used_in_align_clause()
{
  int align = 32; /* { dg-note "'int align' is not const" } */
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align)
    /* { dg-error "the value of 'align' is not usable in a constant expression" "" { target *-*-* } .-1 } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
  };
}

template<typename>
void capture_used_in_align_clause_templ_uninstantiated()
{
  int align = 32; /* { dg-note "'int align' is not const" "" { xfail *-*-* } } */
  auto cl = [align](){
  /* { dg-bogus "the value of 'align' is not usable in a constant expression" "" { target *-*-* } .-1 } */
    int a;
    #pragma omp allocate(a) align(align)
    /* { dg-error "the value of 'align' is not usable in a constant expression" "" { xfail *-*-* } .-1 } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } .-2 } */
  };
}

/* This case can't be diagnosed, there exists a T where align is a converted
   constant expression of long unsigned int.  This is demonstrated
   below in dependent_capture_used_in_align_clause_templ_valid.  */
template<typename T>
void dependent_capture_used_in_align_clause_templ_uninstantiated()
{
  T align = 32; /* { dg-bogus "" } */
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align) /* { dg-bogus "" } */
  };
}

template<typename>
void capture_used_in_align_clause_templ()
{
  int align = 32; /* { dg-note "'int align' is not const" "" { xfail c++17 } } */
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align)
    /* { dg-error "the value of 'align' is not usable in a constant expression" "" { target *-*-* xfail c++17 } .-1 } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* xfail c++17 } .-2 } */
  };
}

template<typename T>
void dependent_capture_used_in_align_clause_templ()
{
  T align = 32; /* { dg-note "'int align' is not const" "" { xfail c++17 } } */
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align)
    /* { dg-error "the value of 'align' is not usable in a constant expression" "" { target *-*-* xfail c++17 } .-1 } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* xfail c++17 } .-2 } */
  };
}

template<typename T>
void dependent_capture_used_in_align_clause_templ_valid()
{
  T align = 32; /* { dg-bogus "" } */
  auto cl = [align](){
    int a;
    #pragma omp allocate(a) align(align) /* { dg-bogus "" } */
  };
}

void instantiate_capture_used_in_align()
{
  capture_used_in_align_clause_templ<void>();
  dependent_capture_used_in_align_clause_templ<int>();

  struct S {
    constexpr S (int) {}
    constexpr operator unsigned int () const { return 32; }
  };
  dependent_capture_used_in_align_clause_templ_valid<S> ();
  dependent_capture_used_in_align_clause_templ_valid<const int> ();
}
