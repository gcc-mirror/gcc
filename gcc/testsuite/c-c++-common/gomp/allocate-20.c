#include "allocate-allocator-handle.h"

#pragma omp allocate() allocator(omp_default_mem_alloc)
/* { dg-error "expected identifier before '\\\)' token" "" { target c } .-1 } */
/* { dg-error "expected unqualified-id before '\\\)' token" "" { target c++ } .-2 } */

/* The following tests are broken up into multiple lines to verify they refer
   to the correct use of the variable, this seems to be the easiest way.
   C does not currently refer to the correct line.  */
int g;
/* { dg-error "'g' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
#pragma omp allocate(\
g,\
g,\
g,\
g,\
g) allocator(omp_default_mem_alloc)
/* { dg-note "appeared first here" "" { target c++ } .-5 } */
/* { dg-error "'g' already appeared as list item in this directive" "" { target c++ } .-5 } */
/* { dg-error "'g' already appeared as list item in this directive" "" { target c++ } .-5 } */
/* { dg-error "'g' already appeared as list item in this directive" "" { target c++ } .-5 } */
/* { dg-error "'g' already appeared as list item in this directive" "" { target c++ } .-5 } */

int g0_0;
int g0_1;
/* { dg-error "'g0_0' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
#pragma omp allocate(\
g0_0,\
g0_1,\
g0_0,\
g0_0) allocator(omp_default_mem_alloc)
/* { dg-note "appeared first here" "" { target c++ } .-4 } */
/* { dg-error "'g0_0' already appeared as list item in this directive" "" { target c++ } .-3 } */
/* { dg-error "'g0_0' already appeared as list item in this directive" "" { target c++ } .-3 } */

int g1_0;
int g1_1;
/* { dg-error "'g1_0' already appeared as list item in an 'allocate' directive" "" { target c } .+2 } */
/* { dg-error "'g1_1' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
#pragma omp allocate(\
g1_1,\
g1_0,\
g1_1,\
g1_0,\
g1_0,\
g1_1) allocator(omp_default_mem_alloc)
/* { dg-note "appeared first here" "" { target c++ } .-6 } */
/* { dg-note "appeared first here" "" { target c++ } .-6 } */
/* { dg-error "'g1_1' already appeared as list item in this directive" "" { target c++ } .-6 } */
/* { dg-error "'g1_0' already appeared as list item in this directive" "" { target c++ } .-6 } */
/* { dg-error "'g1_0' already appeared as list item in this directive" "" { target c++ } .-6 } */
/* { dg-error "'g1_1' already appeared as list item in this directive" "" { target c++ } .-6 } */

void f()
{
  int v;
  /* { dg-error "'v' already appeared as list item in an 'allocate' directive" "" { target c} .+1 } */
  #pragma omp allocate(\
  v,\
  v,\
  v,\
  v,\
  v)
  /* { dg-note "appeared first here" "" { target c++ } .-5 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-5 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-5 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-5 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-5 } */

  int v0_0;
  int v0_1;
  /* { dg-error "'v0_0' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  v0_0,\
  v0_1,\
  v0_0,\
  v0_0)
  /* { dg-note "appeared first here" "" { target c++ } .-4 } */
  /* { dg-error "'v0_0' already appeared as list item in this directive" "" { target c++ } .-3 } */
  /* { dg-error "'v0_0' already appeared as list item in this directive" "" { target c++ } .-3 } */

  int v1_0;
  int v1_1;
  /* { dg-error "'v1_0' already appeared as list item in an 'allocate' directive" "" { target c } .+2 } */
  /* { dg-error "'v1_1' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  v1_1,\
  v1_0,\
  v1_1,\
  v1_0,\
  v1_0,\
  v1_1)
  /* { dg-note "appeared first here" "" { target c++ } .-6 } */
  /* { dg-note "appeared first here" "" { target c++ } .-6 } */
  /* { dg-error "'v1_1' already appeared as list item in this directive" "" { target c++ } .-6 } */
  /* { dg-error "'v1_0' already appeared as list item in this directive" "" { target c++ } .-6 } */
  /* { dg-error "'v1_0' already appeared as list item in this directive" "" { target c++ } .-6 } */
  /* { dg-error "'v1_1' already appeared as list item in this directive" "" { target c++ } .-6 } */
}

void f_with_parm(int p) /* { dg-note "parameter 'p' declared here" "" { target c++ } } */
{
  #pragma omp allocate(p)
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */

  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  p,\
  p,\
  p)
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-3 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-3 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-3 } */

  int v;
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+2 } */
  /* { dg-error "'v' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  v,\
  p,\
  v,\
  v,\
  p,\
  v,\
  v)
  /* { dg-note "appeared first here" "" { target c++ } .-7 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-7 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-7 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-7 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-7 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-7 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-7 } */

  int v0_0;
  int v0_1;
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+2 } */
  /* { dg-error "'v0_0' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  p,\
  v0_0,\
  v0_1,\
  v0_0,\
  v0_0)
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-5 } */
  /* { dg-note "appeared first here" "" { target c++ } .-5 } */
  /* { dg-error "'v0_0' already appeared as list item in this directive" "" { target c++ } .-4 } */
  /* { dg-error "'v0_0' already appeared as list item in this directive" "" { target c++ } .-4 } */

  int v1_0;
  int v1_1;
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+3 } */
  /* { dg-error "'v1_0' already appeared as list item in an 'allocate' directive" "" { target c } .+2 } */
  /* { dg-error "'v1_1' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  v1_1,\
  p,\
  v1_0,\
  v1_1,\
  v1_0,\
  p,\
  v1_0,\
  v1_1,\
  p)
  /* { dg-note "appeared first here" "" { target c++ } .-9 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-9 } */
  /* { dg-note "appeared first here" "" { target c++ } .-9 } */
  /* { dg-error "'v1_1' already appeared as list item in this directive" "" { target c++ } .-9 } */
  /* { dg-error "'v1_0' already appeared as list item in this directive" "" { target c++ } .-9 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-9 } */
  /* { dg-error "'v1_0' already appeared as list item in this directive" "" { target c++ } .-9 } */
  /* { dg-error "'v1_1' already appeared as list item in this directive" "" { target c++ } .-9 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-9 } */
}

/* Valid var used in allocator clause diagnostics.
   (No diagnostic should be emitted related to 'alloc')  */

void f_with_parm_and_allocator0(int p) /* { dg-note "parameter 'p' declared here" "" { target c++ } } */
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  #pragma omp allocate(p) allocator(alloc)
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */

  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  p,\
  p,\
  p) allocator(alloc)
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-3 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-3 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-3 } */

  int v;
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+2 } */
  /* { dg-error "'v' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  v,\
  p,\
  v,\
  v,\
  p,\
  v,\
  v) allocator(alloc)
  /* { dg-note "appeared first here" "" { target c++ } .-7 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-7 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-7 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-7 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-7 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-7 } */
  /* { dg-error "'v' already appeared as list item in this directive" "" { target c++ } .-7 } */

  int v0_0;
  int v0_1;
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+2 } */
  /* { dg-error "'v0_0' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  p,\
  v0_0,\
  v0_1,\
  v0_0,\
  v0_0) allocator(alloc)
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-5 } */
  /* { dg-note "appeared first here" "" { target c++ } .-5 } */
  /* { dg-error "'v0_0' already appeared as list item in this directive" "" { target c++ } .-4 } */
  /* { dg-error "'v0_0' already appeared as list item in this directive" "" { target c++ } .-4 } */

  int v1_0;
  int v1_1;
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+3 } */
  /* { dg-error "'v1_0' already appeared as list item in an 'allocate' directive" "" { target c } .+2 } */
  /* { dg-error "'v1_1' already appeared as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  v1_1,\
  p,\
  v1_0,\
  v1_1,\
  v1_0,\
  p,\
  v1_0,\
  v1_1,\
  p) allocator(alloc)
  /* { dg-note "appeared first here" "" { target c++ } .-9 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-9 } */
  /* { dg-note "appeared first here" "" { target c++ } .-9 } */
  /* { dg-error "'v1_1' already appeared as list item in this directive" "" { target c++ } .-9 } */
  /* { dg-error "'v1_0' already appeared as list item in this directive" "" { target c++ } .-9 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-9 } */
  /* { dg-error "'v1_0' already appeared as list item in this directive" "" { target c++ } .-9 } */
  /* { dg-error "'v1_1' already appeared as list item in this directive" "" { target c++ } .-9 } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-9 } */
}

/* Var used in allocator clause diagnostics.  Tests that invalid vars passed
   into the allocate directive are not considered and bogus/repeat diagnostics
   are not emitted.  */

void f_with_parm_and_allocator1(int p) /* { dg-note "parameter 'p' declared here" "" { target c++ } } */
{
  int v0; /* { dg-note "to be allocated variable declared here" } */
  omp_allocator_handle_t alloc0 = omp_default_mem_alloc; /* { dg-note "declared here" } */
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+2 } */
  /* { dg-error "variable 'alloc0' used in the 'allocator' clause must be declared before 'v0'" "" { target c } .+1 } */
  #pragma omp allocate(\
  p,\
  v0)\
  allocator(alloc0)
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-3 } */
  /* { dg-error "variable 'alloc0' used in the 'allocator' clause must be declared before 'v0'" "" { target c++ } .-2 } */

  int v1; /* { dg-note "declared here" } */
  {
    omp_allocator_handle_t alloc1 = omp_default_mem_alloc;
    int v2;
    /* { dg-error "'allocate' directive must be in the same scope as 'v1'" "" { target c } .+1 } */
    #pragma omp allocate(\
    v1,\
    v2\
    ) allocator(alloc1)
    /* { dg-error "'allocate' directive must be in the same scope as 'v1'" "" { target c++ } .-3 } */
  }
  {
    int v3; /* { dg-note "to be allocated variable declared here" } */
    omp_allocator_handle_t alloc2 = omp_default_mem_alloc; /* { dg-note "declared here" } */
    /* { dg-error "variable 'alloc2' used in the 'allocator' clause must be declared before 'v3'" "" { target c } .+2 } */
    /* { dg-error "'allocate' directive must be in the same scope as 'v1'" "" { target c } .+1 } */
    #pragma omp allocate(\
    v1,\
    v3\
    ) allocator(alloc2)
    /* { dg-error "'allocate' directive must be in the same scope as 'v1'" "" { target c++ } .-3 } */
    /* { dg-error "variable 'alloc2' used in the 'allocator' clause must be declared before 'v3'" "" { target c++ } .-2 } */
  }
}

/* First argument valid.
   These cases could still be fleshed out a bit more, there was original a typo
   that caused diagnostics to always refer to the first argument of the
   directive in the C++ front end, these tests are for that case.  */

void first_valid0()
{
  int a; /* { dg-note "declared here" } */
  {
    int v;
    /* { dg-error "'allocate' directive must be in the same scope as 'a'" "" { target c } .+1 } */
    #pragma omp allocate(\
    v,\
    a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" "" { target c++ } } */
  }
}

void first_valid1(int p) /* { dg-note "parameter 'p' declared here" "" { target c++ } } */
{
  int v;
  /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+1 } */
  #pragma omp allocate(\
  v,\
  p) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } } */
}

void first_valid2(int p) /* { dg-note "parameter 'p' declared here" "" { target c++ } } */
{
  int a; /* { dg-note "declared here" } */
  {
    int v;
    /* { dg-error "'allocate' directive must be in the same scope as 'a'" "" { target c } .+2 } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c } .+1 } */
    #pragma omp allocate(\
    v,\
    a,\
    p)
    /* { dg-error "'allocate' directive must be in the same scope as 'a'" "" { target c++ } .-2 } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target c++ } .-2 } */
  }
}

/* Missing cases that contain undeclared variables.  */
