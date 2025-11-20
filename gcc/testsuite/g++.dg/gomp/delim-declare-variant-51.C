/* { dg-do compile { target c++11 } } */

/* Test delimited declare variant on constexpr, deleted, and defaulted
   functions.  */
/* C++11 */

/* TODO: add templates cases for constexpr/delete free functions */

/* Do we warn for the mismatch?
   TBH we probably warn whenever a variant function is constexpr in general.
   I can't imagine that we are going to support constant evaluation of a
   variant function, realistically the only choice is to always use the base
   function if a constant-expression is required.  */
constexpr int freefn_mismatched_constexpr_before0 () { return 0; }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
int freefn_mismatched_constexpr_before0 () { return 1; }
#pragma omp end declare variant

int freefn_mismatched_constexpr_before1 () { return 0; }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
constexpr int freefn_mismatched_constexpr_before1 () { return 1; }
#pragma omp end declare variant

#pragma omp begin declare variant match (implementation={vendor("gnu")})
constexpr int freefn_mismatched_constexpr_after0 () { return 1; }
#pragma omp end declare variant
int freefn_mismatched_constexpr_after0 () { return 0; }

#pragma omp begin declare variant match (implementation={vendor("gnu")})
int freefn_mismatched_constexpr_after1 () { return 1; }
#pragma omp end declare variant
constexpr int freefn_mismatched_constexpr_after1 () { return 0; }



void freefn_deleted_before () = delete;
#pragma omp begin declare variant match (implementation={vendor("gnu")})
void freefn_deleted_before () {}  // { dg-error "declare variant directives are not allowed on deleted functions" }
#pragma omp end declare variant

#pragma omp begin declare variant match (implementation={vendor("gnu")})
void freefn_deleted_after () {}  // { dg-error "declare variant directives are not allowed on deleted functions" }
#pragma omp end declare variant
void freefn_deleted_after () = delete;

/* TECHNICALLY allowed by the spec, but obviously conflicts with the intention.  */
void freefn_variant_deleted_base_before () {}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
void freefn_variant_deleted_base_before () = delete;  // { dg-error "declare variant directives are not allowed on deleted functions" }
#pragma omp end declare variant

#pragma omp begin declare variant match (implementation={vendor("gnu")})
void freefn_variant_deleted_base_after () = delete;  // { dg-error "declare variant directives are not allowed on deleted functions" }
#pragma omp end declare variant
void freefn_variant_deleted_base_after () {};


/* For now, obviously error, not sure if we error on just the base or on
   both though.
   In the future, I think if the base and all variants are deleted, we can
   treat a call to the function as deleted before we determine a variant.  */
void freefn_both_deleted_base_before () = delete;
#pragma omp begin declare variant match (implementation={vendor("gnu")})
void freefn_both_deleted_base_before () = delete;  // { dg-error "declare variant directives are not allowed on deleted functions" }
#pragma omp end declare variant

#pragma omp begin declare variant match (implementation={vendor("gnu")})
void freefn_both_deleted_base_after () = delete;  // { dg-error "declare variant directives are not allowed on deleted functions" }
#pragma omp end declare variant
void freefn_both_deleted_base_after () = delete;




struct S0
{
  void f_deleted_before () = delete;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  void f_deleted_before () {}  // { dg-error "declare variant directives are not allowed on deleted functions" }
  #pragma omp end declare variant

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  void f_deleted_after () {}  // { dg-error "declare variant directives are not allowed on deleted functions" }
  #pragma omp end declare variant
  void f_deleted_after () = delete;
};


/* These should error for constructor/destructor, not default.  */
struct S_default_before {
  S_default_before() = default;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_before() {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant

  S_default_before(S_default_before const&) = default;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_before(S_default_before const&) {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant

  S_default_before(S_default_before&&) = default;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_before(S_default_before&&) {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant

  ~S_default_before() = default;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  ~S_default_before() {}  // { dg-error "declare variant directives are not allowed on destructors" }
  #pragma omp end declare variant
};

struct S_default_after {
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_after() {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant
  S_default_after() = default;

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_after(S_default_after const&) {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant
  S_default_after(S_default_after const&) = default;

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_after(S_default_after&&) {}  // { dg-error "declare variant directives are not allowed on constructors" }
  #pragma omp end declare variant
  S_default_after(S_default_after&&) = default;

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  ~S_default_after() {}  // { dg-error "declare variant directives are not allowed on destructors" }
  #pragma omp end declare variant
  ~S_default_after() = default;
};

/* These should error for default/delete.  */
struct S_default_assignment_before {
  S_default_assignment_before& operator=(S_default_assignment_before const&) = default;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_assignment_before& operator=(S_default_assignment_before const&) { return *this; }  // { dg-error "declare variant directives are not allowed on defaulted functions" }
  #pragma omp end declare variant

  S_default_assignment_before& operator=(S_default_assignment_before&&) = default;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_assignment_before& operator=(S_default_assignment_before&&) { return *this; }  // { dg-error "declare variant directives are not allowed on defaulted functions" }
  #pragma omp end declare variant
};

struct S_default_assignment_after {
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_assignment_after& operator=(S_default_assignment_after const&) { return *this; }  // { dg-error "declare variant directives are not allowed on defaulted functions" }
  #pragma omp end declare variant
  S_default_assignment_after& operator=(S_default_assignment_after const&) = default;

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_default_assignment_after& operator=(S_default_assignment_after&&) { return *this; }  // { dg-error "declare variant directives are not allowed on defaulted functions" }
  #pragma omp end declare variant
  S_default_assignment_after& operator=(S_default_assignment_after&&) = default;
};

struct S_deleted_assignment_before {
  S_deleted_assignment_before& operator=(S_deleted_assignment_before const&) = delete;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_deleted_assignment_before& operator=(S_deleted_assignment_before const&) { return *this; }  // { dg-error "declare variant directives are not allowed on deleted functions" }
  #pragma omp end declare variant

  S_deleted_assignment_before& operator=(S_deleted_assignment_before&&) = delete;
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_deleted_assignment_before& operator=(S_deleted_assignment_before&&) { return *this; }  // { dg-error "declare variant directives are not allowed on deleted functions" }
  #pragma omp end declare variant
};

struct S_deleted_assignment_after {
  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_deleted_assignment_after& operator=(S_deleted_assignment_after const&) { return *this; }  // { dg-error "declare variant directives are not allowed on deleted functions" }
  #pragma omp end declare variant
  S_deleted_assignment_after& operator=(S_deleted_assignment_after const&) = delete;

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  S_deleted_assignment_after& operator=(S_deleted_assignment_after&&) { return *this; }  // { dg-error "declare variant directives are not allowed on deleted functions" }
  #pragma omp end declare variant
  S_deleted_assignment_after& operator=(S_deleted_assignment_after&&) = delete;
};
