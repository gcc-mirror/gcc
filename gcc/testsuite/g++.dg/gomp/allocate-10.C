#include "allocate-allocator-handle.h"

/* Diagnostics for invalid cases.  There are a few valid cases peppered in here
   but that's not what is being tested for in here.  */


/****************************************************
 * Reference variable used in an allocate directive *
 ****************************************************/

void ref_var()
{
  int a = 42;
  int& ref = a; /* { dg-note "'ref' declared here" } */
  #pragma omp allocate(ref) /* { dg-error "variable 'ref' with reference type may not appear as a list item in an 'allocate' directive" } */
}

template<typename>
void ref_var_templ_not_instantiated()
{
  int a = 42;
  int& ref = a; /* { dg-note "'ref' declared here" } */
  #pragma omp allocate(ref) /* { dg-error "variable 'ref' with reference type may not appear as a list item in an 'allocate' directive" } */
}

template<typename T>
void dependent_ref_var_templ_not_instantiated()
{
  T a = 42;
  T& t = a; /* { dg-note "'t' declared here" } */
  #pragma omp allocate(t) /* { dg-error "variable 't' with reference type may not appear as a list item in an 'allocate' directive" } */
}

template<typename T>
void dependent_var_templ_not_instantiated()
{
  T t = 42;
  #pragma omp allocate(t)
}

template<typename T>
void dependent_var_templ_0()
{
  T t = 42;
  #pragma omp allocate(t)
}

template<typename T>
void dependent_var_templ_1()
{
  T t = 42; /* { dg-note "'t' declared here" } */
  #pragma omp allocate(t) /* { dg-error "variable 't' with reference type may not appear as a list item in an 'allocate' directive" } */
}

template<typename T>
void dependent_var_templ_2()
{
  int a;
  T t = a; /* { dg-note "'t' declared here" } */
  #pragma omp allocate(t) /* { dg-error "variable 't' with reference type may not appear as a list item in an 'allocate' directive" } */
}

void instantiate_var_templ()
{
  dependent_var_templ_0<int>(); /* { dg-bogus "required from here" } */
  dependent_var_templ_1<int>(); /* { dg-bogus "required from here" } */
  dependent_var_templ_1<int const&>(); /* { dg-message "required from here" } */
  dependent_var_templ_2<int>(); /* { dg-bogus "required from here" } */
  dependent_var_templ_2<int&>(); /* { dg-message "required from here" } */
  dependent_var_templ_2<int const&>(); /* { dg-message "required from here" } */
}


/****************************
 * Invalid allocator clause *
 ****************************/

template<omp_allocator_handle_t Alloc>
void nttp_allocator()
{
  int a;
  #pragma omp allocate(a) allocator(Alloc)
}

template<omp_allocator_handle_t Alloc>
void nttp_allocator_uninstantiated()
{
  int a;
  #pragma omp allocate(a) allocator(Alloc)
}

template<int Alloc>
void nttp_wrong_type_allocator_uninstantiated()
{
  int a;
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" } */
}

template<typename AllocT, AllocT Alloc>
void nttp_dependent_type_allocator()
{
  int a;
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" } */
}

template<typename AllocT, AllocT Alloc>
void nttp_dependent_type_allocator_uninstantiated()
{
  int a;
  #pragma omp allocate(a) allocator(Alloc)
}

void instantiate_nttp_allocator()
{
  nttp_allocator<omp_default_mem_alloc>(); /* { dg-bogus "required from here" } */
  nttp_dependent_type_allocator<omp_allocator_handle_t, omp_default_mem_alloc>(); /* { dg-bogus "required from here" } */
  nttp_dependent_type_allocator<int, 5>(); /* { dg-message "required from here" } */
}

template<omp_allocator_handle_t Alloc>
void nttp_allocator_static()
{
  static int a; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" } */
}

template<omp_allocator_handle_t Alloc>
void nttp_allocator_uninstantiated_static()
{
  static int a;
  #pragma omp allocate(a) allocator(Alloc)
}

template<int Alloc>
void nttp_wrong_type_allocator_uninstantiated_static()
{
  static int a;
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" } */
}

template<typename AllocT, AllocT Alloc>
void nttp_dependent_type_allocator_static_0()
{
  static int a; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" } */
}

template<typename AllocT, AllocT Alloc>
void nttp_dependent_type_allocator_static_1()
{
  static int a;
  #pragma omp allocate(a) allocator(Alloc) /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" } */
}

template<typename AllocT, AllocT Alloc>
void nttp_dependent_type_allocator_uninstantiated_static()
{
  static int a;
  #pragma omp allocate(a) allocator(Alloc)
}

#define DEFINITELY_NOT_PREDEFINED static_cast<omp_allocator_handle_t>(1024)

void instantiate_nttp_allocator_static()
{
  nttp_allocator_static<omp_default_mem_alloc>(); /* { dg-bogus "required from here" } */
  nttp_allocator_static<DEFINITELY_NOT_PREDEFINED>(); /* { dg-message "required from here" } */
  nttp_dependent_type_allocator_static_0<omp_allocator_handle_t, omp_default_mem_alloc>(); /* { dg-bogus "required from here" } */
  nttp_dependent_type_allocator_static_0<omp_allocator_handle_t, DEFINITELY_NOT_PREDEFINED>(); /* { dg-message "required from here" } */
  nttp_dependent_type_allocator_static_1<int, 1>(); /* { dg-message "required from here" } */
}

#undef DEFINITELY_NOT_PREDEFINED


template<typename AllocT>
void templ_allocator_param_0(AllocT alloc)
{
  int a;
  #pragma omp allocate(a) allocator(alloc)
}

template<typename AllocT>
void templ_allocator_param_1(AllocT alloc)
{
  int a;
  #pragma omp allocate(a) allocator(alloc) /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" } */
}

template<typename AllocT>
void templ_allocator_param_uninstantiated(AllocT alloc)
{
  int a;
  #pragma omp allocate(a) allocator(alloc)
}

void instantiate_templ_allocator_param()
{
  templ_allocator_param_0(omp_default_mem_alloc); /* { dg-bogus "required from here" } */
  templ_allocator_param_1(omp_default_mem_alloc); /* { dg-bogus "required from here" } */
  templ_allocator_param_1(0); /* { dg-message "required from here" } */
}


template<typename>
void missing_allocator_clause_uninstantiated()
{
  static int a; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) /* { dg-error "'allocator' clause required for static variable 'a'" } */
}

/* Cases that are never constant omp_allocator_handle_t expressions (and are required to be) */

template<typename>
void allocator_param_static_uninstantiated(omp_allocator_handle_t alloc)
{
  static int a; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) allocator(alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" } */
}

template<typename>
void allocator_var_static_uninstantiated()
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  static int a; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) allocator(alloc) /* { dg-error "'allocator' clause requires a predefined allocator as 'a' is static" } */
}

/* See cp/semantics.cc:finish_omp_allocate
   These cases will always be invalid but diagnosing type dependent cases
   before instantiation is too difficult.  */

template<typename AllocT>
void templ_allocator_param_static_uninstantiated(AllocT alloc)
{
  static int a;
  #pragma omp allocate(a) allocator(alloc)
}

template<typename AllocT>
void templ_allocator_var_static_uninstantiated()
{
  AllocT alloc = omp_default_mem_alloc;
  static int a;
  #pragma omp allocate(a) allocator(alloc)
}


/************************
 * Invalid align clause *
 ************************/

template<int Align>
void nttp_align()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
}

template<int Align>
void nttp_align_uninstantiated()
{
  int a;
  #pragma omp allocate(a) align(Align)
}

template<int* Align>
void nttp_wrong_type_align_uninstantiated()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
}

template<typename AlignT, AlignT Align>
void nttp_dependent_type_align_0()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
}

template<typename AlignT, AlignT Align>
void nttp_dependent_type_align_1()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
}

template<typename AlignT, AlignT Align>
void nttp_dependent_type_align_uninstantiated()
{
  int a;
  #pragma omp allocate(a) align(Align)
}

void instantiate_nttp_align()
{
  nttp_align<32>();
  nttp_align<42>(); /* { dg-message "required from here" } */
  nttp_dependent_type_align_0<int, 32>(); /* { dg-bogus "required from here" } */
  nttp_dependent_type_align_0<int, 42>(); /* { dg-message "required from here" } */
  nttp_dependent_type_align_1<int, 32>(); /* { dg-bogus "required from here" } */
  /* We just need any non integer NTTP that is valid in c++98, this fits the bill.  */
  nttp_dependent_type_align_1<void(*)(), instantiate_nttp_align>(); /* { dg-message "required from here" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "Bugged diagnostic, see comment" { target *-*-* } .-1 } */
  /* I believe this diagnostic is bugged, it should refer to where the
     expression is used, not where it originated from.  This isn't a bug for
     this feature though so I'm making the test case work around it,
     when this bug is fixed this test case, and the xfail in the test case in
     nttp_dependent_type_align_1 can be remove.  */
}

/* Cases that are never constant integer expressions (always required for the align clause) */

template<typename>
void align_param_uninstantiated(int align)
{
  int a;
  #pragma omp allocate(a) align(align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
}

template<typename>
void align_var_uninstantiated()
{
  int align = 32;
  int a;
  #pragma omp allocate(a) align(align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
}

/* See cp/semantics.cc:finish_omp_allocate
   These cases will always be invalid but diagnosing type dependent cases
   before instantiation is too difficult.  */

template<typename AlignT>
void templ_align_param_uninstantiated(AlignT align)
{
  int a;
  #pragma omp allocate(a) align(align)
}

template<typename AlignT>
void templ_align_var_uninstantiated()
{
  AlignT align = 32;
  int a;
  #pragma omp allocate(a) align(align)
}



/***************
 * Mixed cases *
 ***************/

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_uninstantiated()
{
  int b = 42;
  Var a = b;
  #pragma omp allocate(a) allocator(Alloc) align(Align)
}

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_valid()
{
  int b = 42;
  Var a = b;
  #pragma omp allocate(a) allocator(Alloc) align(Align)
}

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_0()
{
  int b = 42;
  Var a = b; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) allocator(Alloc) align(Align)
  /* { dg-error "variable 'a' with reference type may not appear as a list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
}

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_1()
{
  int b = 42;
  Var a = b;
  #pragma omp allocate(a) allocator(Alloc) align(Align)
  /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" "" { target *-*-* } .-1 } */
}

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_2()
{
  int b = 42;
  Var a = b;
  #pragma omp allocate(a) allocator(Alloc) align(Align)
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_3()
{
  int b = 42;
  Var a = b; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) allocator(Alloc) align(Align)
  /* { dg-error "variable 'a' with reference type may not appear as a list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" "" { target *-*-* } .-2 } */
}

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_4()
{
  int b = 42;
  Var a = b; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) allocator(Alloc) align(Align)
  /* { dg-error "variable 'a' with reference type may not appear as a list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
}

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_5()
{
  int b = 42;
  Var a = b;
  #pragma omp allocate(a) allocator(Alloc) align(Align)
  /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" "" { target *-*-* } .-1 } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
}

template<typename Var,
  typename AllocT, AllocT Alloc,
  typename AlignT, AlignT Align>
void all_dependent_6()
{
  int b = 42;
  Var a = b; /* { dg-note "'a' declared here" } */
  #pragma omp allocate(a) allocator(Alloc) align(Align)
  /* { dg-error "variable 'a' with reference type may not appear as a list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  /* { dg-error "'allocator' clause expression has type 'int' rather than 'omp_allocator_handle_t'" "" { target *-*-* } .-2 } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-3 } */
}

void instantiate_all_dependent()
{
  all_dependent_valid<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>();
  /* Don't test the type mismatch for the align clause here, it's diagnostic
     location is buggy, and the error message is the same.  We just really want
     to test that we aren't emitting bogus errors when multiple things are
     dependent, so it's unnecessary to test that case again.  */
  all_dependent_0<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>(); /* { dg-bogus "required from here" } */
  all_dependent_0<int&, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>(); /* { dg-message "required from here" } */

  all_dependent_1<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>(); /* { dg-bogus "required from here" } */
  all_dependent_1<int, int, 1, int, 32>(); /* { dg-message "required from here" } */

  all_dependent_2<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>(); /* { dg-bogus "required from here" } */
  all_dependent_2<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 42>(); /* { dg-message "required from here" } */

  all_dependent_3<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>(); /* { dg-bogus "required from here" } */
  all_dependent_3<int&, int, 1, int, 32>(); /* { dg-message "required from here" } */

  all_dependent_4<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>(); /* { dg-bogus "required from here" } */
  all_dependent_4<int&, omp_allocator_handle_t, omp_default_mem_alloc, int, 42>(); /* { dg-message "required from here" } */

  all_dependent_5<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>(); /* { dg-bogus "required from here" } */
  all_dependent_5<int, int, 1, int, 42>(); /* { dg-message "required from here" } */

  all_dependent_6<int, omp_allocator_handle_t, omp_default_mem_alloc, int, 32>(); /* { dg-bogus "required from here" } */
  all_dependent_6<int&, int, 1, int, 42>(); /* { dg-message "required from here" } */
}

/* We are missing combined cases for static var used in the allocate directive,
   but it should be fine, the combined cases immediately above are probably
   overkill as it is.  */


/******************************
 * Invalid allocate directive *
 ******************************/

/* We are only testing that we gracefully handle an empty list of vars.  */

void no_parens()
{
  #pragma omp allocate /* { dg-error "expected '\\\(' before end of line" } */
}

template<typename>
void templ_no_parens()
{
  #pragma omp allocate /* { dg-error "expected '\\\(' before end of line" } */
}
template void templ_no_parens<void>();

template<typename>
void templ_no_parens_uninstantiated()
{
  #pragma omp allocate /* { dg-error "expected '\\\(' before end of line" } */
}

void no_vars()
{
  #pragma omp allocate() /* { dg-error "expected unqualified-id before '\\\)' token" } */
}

template<typename>
void templ_no_vars()
{
  #pragma omp allocate() /* { dg-error "expected unqualified-id before '\\\)' token" } */
}
template void templ_no_vars<void>();

template<typename>
void templ_no_vars_uninstantiated()
{
  #pragma omp allocate() /* { dg-error "expected unqualified-id before '\\\)' token" } */
}

/* We can't diagnose anything about the allocator clause if we have no
   variables, but we do need to make sure we don't crash.  */

void no_vars_allocator()
{
  #pragma omp allocate() allocator(omp_default_mem_alloc) /* { dg-error "expected unqualified-id before '\\\)' token" } */
}

template<typename>
void templ_no_vars_allocator()
{
  #pragma omp allocate() allocator(omp_default_mem_alloc) /* { dg-error "expected unqualified-id before '\\\)' token" } */
}
template void templ_no_vars_allocator<void>();

template<typename>
void templ_no_vars_allocator_uninstantiated()
{
  #pragma omp allocate() allocator(omp_default_mem_alloc) /* { dg-error "expected unqualified-id before '\\\)' token" } */
}

/* We can still diagnose errors about the align clause without any vars.  */

void no_vars_invalid_align()
{
  #pragma omp allocate() align(42) /* { dg-error "expected unqualified-id before '\\\)' token" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}

template<typename>
void templ_no_vars_invalid_align()
{
  #pragma omp allocate() align(42) /* { dg-error "expected unqualified-id before '\\\)' token" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}
template void templ_no_vars_invalid_align<void>();

template<typename>
void templ_no_vars_invalid_align_uninstantiated()
{
  #pragma omp allocate() align(42) /* { dg-error "expected unqualified-id before '\\\)' token" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}

template<int Align>
void templ_no_vars_dep_align()
{
  #pragma omp allocate() align(Align) /* { dg-error "expected unqualified-id before '\\\)' token" } */
}
template void templ_no_vars_dep_align<32>();

template<int Align>
void templ_no_vars_dep_align_invalid()
{
  #pragma omp allocate() align(Align) /* { dg-error "expected unqualified-id before '\\\)' token" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}
template void templ_no_vars_dep_align_invalid<42>();

template<int Align>
void templ_no_vars_dep_align_uninstantiated()
{
  #pragma omp allocate() align(Align) /* { dg-error "expected unqualified-id before '\\\)' token" } */
}

/*********************************
 * All vars in directive invalid *
 *********************************/

void invalid_vars_param(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
}

template<typename>
void templ_invalid_vars_param(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
}
template void templ_invalid_vars_param<void>(int);

template<typename>
void templ_invalid_vars_param_uninstantiated(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
}

void invalid_vars_out_of_scope()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  }
}

template<typename>
void templ_invalid_vars_out_of_scope()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  }
}
template void templ_invalid_vars_out_of_scope<void>();

template<typename>
void templ_invalid_vars_out_of_scope_uninstantiated()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  }
}

void invalid_vars_out_of_scope_and_param(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  }
}

template<typename>
void templ_invalid_vars_out_of_scope_and_param(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  }
}
template void templ_invalid_vars_out_of_scope_and_param<void>(int);

template<typename>
void templ_invalid_vars_out_of_scope_and_param_uninstantiated(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  }
}

/* Same as above, we can't diagnose anything about the allocator clause if we
   have no variables, but we do need to make sure we don't crash.  */

void invalid_vars_param_allocator(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) allocator(omp_default_mem_alloc) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
}

template<typename>
void templ_invalid_vars_param_allocator(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) allocator(omp_default_mem_alloc) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
}
template void templ_invalid_vars_param_allocator<void>(int);

template<typename>
void templ_invalid_vars_param_allocator_uninstantiated(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) allocator(omp_default_mem_alloc) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
}

void invalid_vars_out_of_scope_allocator()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) allocator(omp_default_mem_alloc) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  }
}

template<typename>
void templ_invalid_vars_out_of_scope_allocator()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) allocator(omp_default_mem_alloc) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  }
}
template void templ_invalid_vars_out_of_scope_allocator<void>();

template<typename>
void templ_invalid_vars_out_of_scope_allocator_uninstantiated()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) allocator(omp_default_mem_alloc) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  }
}

void invalid_vars_out_of_scope_and_param_allocator(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) allocator(omp_default_mem_alloc) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  }
}

template<typename>
void templ_invalid_vars_out_of_scope_and_param_allocator(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) allocator(omp_default_mem_alloc) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  }
}
template void templ_invalid_vars_out_of_scope_and_param_allocator<void>(int);

template<typename>
void templ_invalid_vars_out_of_scope_and_param_allocator_uninstantiated(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) allocator(omp_default_mem_alloc) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  }
}

/* Invalid vars with non-dependent invalid align */

void invalid_vars_param_align_invalid(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) align(42) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}

template<typename>
void templ_invalid_vars_param_align_invalid(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) align(42) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}
template void templ_invalid_vars_param_align_invalid<void>(int);

template<typename>
void templ_invalid_vars_param_align_invalid_uninstantiated(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) align(42) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}

void invalid_vars_out_of_scope_align_invalid()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) align(42) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
  }
}

template<typename>
void templ_invalid_vars_out_of_scope_align_invalid()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) align(42) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
  }
}
template void templ_invalid_vars_out_of_scope_align_invalid<void>();

template<typename>
void templ_invalid_vars_out_of_scope_align_invalid_uninstantiated()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) align(42) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
  }
}

void invalid_vars_out_of_scope_and_param_align_invalid(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) align(42) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
  }
}

template<typename>
void templ_invalid_vars_out_of_scope_and_param_align_invalid(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) align(42) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
  }
}
template void templ_invalid_vars_out_of_scope_and_param_align_invalid<void>(int);

template<typename>
void templ_invalid_vars_out_of_scope_and_param_align_invalid_uninstantiated(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) align(42) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
  }
}


/* Param (dependent align) */

template<int Align>
void templ_invalid_vars_param_dependent_align_uninstantiated(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) align(Align) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
}

template<int Align>
void templ_invalid_vars_param_dependent_align(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) align(Align) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
}
template void templ_invalid_vars_param_dependent_align<32>(int);

template<int Align>
void templ_invalid_vars_param_dependent_align_invalid(int p) /* { dg-note "parameter 'p' declared here" } */
{
  #pragma omp allocate(p) align(Align) /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}
template void templ_invalid_vars_param_dependent_align_invalid<42>(int);


/* Out of scope (dependent align) */

template<int Align>
void templ_invalid_vars_out_of_scope_dependent_align_uninstantiated()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) align(Align) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  }
}

template<int Align>
void templ_invalid_vars_out_of_scope_dependent_align()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) align(Align) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
  }
}
template void templ_invalid_vars_out_of_scope_dependent_align<32>();

template<int Align>
void templ_invalid_vars_out_of_scope_dependent_align_invalid()
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a) align(Align) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
  }
}
template void templ_invalid_vars_out_of_scope_dependent_align_invalid<42>();


/* Param and out of scope (dependent align) */

template<int Align>
void templ_invalid_vars_out_of_scope_and_param_dependent_align_uninstantiated(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) align(Align) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  }
}

template<int Align>
void templ_invalid_vars_out_of_scope_and_param_dependent_align(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) align(Align) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
  }
}
template void templ_invalid_vars_out_of_scope_and_param_dependent_align<32>(int);

template<int Align>
void templ_invalid_vars_out_of_scope_and_param_dependent_align_invalid(int p) /* { dg-note "parameter 'p' declared here" } */
{
  int a; /* { dg-note "declared here" } */
  {
    #pragma omp allocate(a, p) align(Align) /* { dg-error "'allocate' directive must be in the same scope as 'a'" } */
    /* { dg-error "function parameter 'p' may not appear as list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
    /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
  }
}
template void templ_invalid_vars_out_of_scope_and_param_dependent_align_invalid<42>(int);



/****************************************************
 * uses of var in multiple directives in a template *
 ****************************************************/

/* We are missing a lot of cases here but testing all of them shouldn't be
   necessary.  Uses of variables in multiple directives are diagnosed during
   parsing so templates shouldn't change anything.  This is of course as long
   as we don't change that, and these cases should be enough to deter anyone
   from doing so.  */

template<typename>
void multiple_uses_non_dependent_directive_uninstantiated()
{
  int a;
  #pragma omp allocate(a) /* { dg-note "'a' previously appeared here" } */
  #pragma omp allocate(a) /* { dg-error "'a' already appeared as list item in an 'allocate' directive" } */
}

template<typename>
void multiple_uses_non_dependent_directive()
{
  int a;
  #pragma omp allocate(a) /* { dg-note "'a' previously appeared here" } */
  #pragma omp allocate(a) /* { dg-error "'a' already appeared as list item in an 'allocate' directive" } */
}
template void multiple_uses_non_dependent_directive<void>();


template<int Align>
void multiple_uses_dep_directive_before_align_uninstantiated()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-note "'a' previously appeared here" } */
  #pragma omp allocate(a) /* { dg-error "'a' already appeared as list item in an 'allocate' directive" } */
}

template<int Align>
void multiple_uses_dep_directive_before_valid_align()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-note "'a' previously appeared here" } */
  #pragma omp allocate(a) /* { dg-error "'a' already appeared as list item in an 'allocate' directive" } */
}
template void multiple_uses_dep_directive_before_valid_align<32>();

template<int Align>
void multiple_uses_dep_directive_before_invalid_align()
{
  int a;
  #pragma omp allocate(a) align(Align) /* { dg-note "'a' previously appeared here" } */
  #pragma omp allocate(a) /* { dg-error "'a' already appeared as list item in an 'allocate' directive" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
}
template void multiple_uses_dep_directive_before_invalid_align<42>();


/* Dependent directive after the independent one.  */

template<int Align>
void multiple_uses_dep_directive_after_align_uninstantiated()
{
  int a;
  #pragma omp allocate(a) /* { dg-note "'a' previously appeared here" } */
  #pragma omp allocate(a) align(Align) /* { dg-error "'a' already appeared as list item in an 'allocate' directive" } */
}

template<int Align>
void multiple_uses_dep_directive_after_valid_align()
{
  int a;
  #pragma omp allocate(a) /* { dg-note "'a' previously appeared here" } */
  #pragma omp allocate(a) align(Align) /* { dg-error "'a' already appeared as list item in an 'allocate' directive" } */
}
template void multiple_uses_dep_directive_after_valid_align<32>();

template<int Align>
void multiple_uses_dep_directive_after_invalid_align()
{
  int a;
  #pragma omp allocate(a) /* { dg-note "'a' previously appeared here" } */
  #pragma omp allocate(a) align(Align) /* { dg-error "'a' already appeared as list item in an 'allocate' directive" } */
  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
}
template void multiple_uses_dep_directive_after_invalid_align<42>();

