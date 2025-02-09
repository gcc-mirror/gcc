/* { dg-do compile { target c++20 } } */

/* The procedure that a declare variant directive determined to be a function
   variant may not be an immediate function + Declare variant directives may
   not be specified for immediate functions. */
consteval void freefn_consteval_before0 () {}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
consteval void freefn_consteval_before0 () {}  // { dg-error "declare variant directives are not allowed on immediate functions" }
#pragma omp end declare variant

/* Declare variant directives may not be specified for immediate functions.  */
consteval void freefn_consteval_before1 () {}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
void freefn_consteval_before1 () {}  // { dg-error "declare variant directives are not allowed on immediate functions" }
#pragma omp end declare variant

/* The procedure that a declare variant directive determined to be a function
   variant may not be an immediate function.  */
void freefn_consteval_before2 () {}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
consteval void freefn_consteval_before2 () {}  // { dg-error "declare variant directives are not allowed on immediate functions" }
#pragma omp end declare variant


