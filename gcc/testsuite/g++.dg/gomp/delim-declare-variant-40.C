// { dg-do compile }

// Check that variants for a template function are instantiated correctly.
// FIXME:  Fails due to PR118530.  

template<typename T>
void f_default_param (T = 42) {}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void f_default_param (T = 42) {}
#pragma omp end declare variant

template<typename T>
void f_no_param () {}  // { dg-bogus "no matching function for call" "PR118530" { xfail *-*-* } }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void f_no_param () {}
#pragma omp end declare variant

void instantiate_f()
{
  f_default_param<int>();
  f_no_param<int>();
}

template<int>
void nttp () {}  // { dg-bogus "no matching function for call" "PR118530" { xfail *-*-* } }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<int>
void nttp () {}
#pragma omp end declare variant

void instantiate_nttp()
{
  nttp<42>();
}

template<typename>
struct S {};

template<template<typename> class Templ>
void templ_templ () {}  // { dg-bogus "no matching function for call" "PR118530" { xfail *-*-* } }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<template<typename> class Templ>
void templ_templ () {}
#pragma omp end declare variant

void instantiate_templ_templ()
{
  templ_templ<S>();
}
