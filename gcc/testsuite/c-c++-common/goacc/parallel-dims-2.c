/* Invalid use of OpenACC parallelism dimensions clauses: 'num_gangs',
   'num_workers', 'vector_length'.  */

/* See also '../../gfortran.dg/goacc/parallel-dims-2.f90'.  */

void f(int i, float f)
{
#pragma acc kernels num_gangs /* { dg-error "expected '\\(' before end of line" } */
  ;
#pragma acc kernels num_workers /* { dg-error "expected '\\(' before end of line" } */
  ;
#pragma acc kernels vector_length /* { dg-error "expected '\\(' before end of line" } */
  ;

#pragma acc parallel num_gangs /* { dg-error "expected '\\(' before end of line" } */
  ;
#pragma acc parallel num_workers /* { dg-error "expected '\\(' before end of line" } */
  ;
#pragma acc parallel vector_length /* { dg-error "expected '\\(' before end of line" } */
  ;


#pragma acc kernels num_gangs( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;
#pragma acc kernels num_workers( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;
#pragma acc kernels vector_length( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;

#pragma acc parallel num_gangs( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;
#pragma acc parallel num_workers( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;
#pragma acc parallel vector_length( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;


#pragma acc kernels num_gangs() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;
#pragma acc kernels num_workers() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;
#pragma acc kernels vector_length() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;

#pragma acc parallel num_gangs() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;
#pragma acc parallel num_workers() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;
#pragma acc parallel vector_length() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;


#pragma acc kernels num_gangs(1 /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc kernels num_workers(1 /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc kernels vector_length(1 /* { dg-error "expected '\\)' before end of line" } */
  ;

#pragma acc parallel num_gangs(1 /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc parallel num_workers(1 /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc parallel vector_length(1 /* { dg-error "expected '\\)' before end of line" } */
  ;


#pragma acc kernels num_gangs(i /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc kernels num_workers(i /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc kernels vector_length(i /* { dg-error "expected '\\)' before end of line" } */
  ;

#pragma acc parallel num_gangs(i /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc parallel num_workers(i /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc parallel vector_length(i /* { dg-error "expected '\\)' before end of line" } */
  ;


#pragma acc kernels num_gangs(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc kernels num_workers(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc kernels vector_length(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;

#pragma acc parallel num_gangs(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc parallel num_workers(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc parallel vector_length(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;


#pragma acc kernels num_gangs(1 i) /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc kernels num_workers(1 i) /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc kernels vector_length(1 i) /* { dg-error "expected '\\)' before 'i'" } */
  ;

#pragma acc parallel num_gangs(1 i) /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc parallel num_workers(1 i) /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc parallel vector_length(1 i) /* { dg-error "expected '\\)' before 'i'" } */
  ;


#pragma acc kernels num_gangs(1, i /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  /* { dg-bogus "expected '\\)' before end of line" "TODO" { xfail c } .-1 } */
  ;
#pragma acc kernels num_workers(1, i /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  /* { dg-bogus "expected '\\)' before end of line" "TODO" { xfail c } .-1 } */
  ;
#pragma acc kernels vector_length(1, i /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  /* { dg-bogus "expected '\\)' before end of line" "TODO" { xfail c } .-1 } */
  ;

#pragma acc parallel num_gangs(1, i /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  /* { dg-bogus "expected '\\)' before end of line" "TODO" { xfail c } .-1 } */
  ;
#pragma acc parallel num_workers(1, i /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  /* { dg-bogus "expected '\\)' before end of line" "TODO" { xfail c } .-1 } */
  ;
#pragma acc parallel vector_length(1, i /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  /* { dg-bogus "expected '\\)' before end of line" "TODO" { xfail c } .-1 } */
  ;


#pragma acc kernels num_gangs(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;
#pragma acc kernels num_workers(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;
#pragma acc kernels vector_length(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;

#pragma acc parallel num_gangs(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;
#pragma acc parallel num_workers(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;
#pragma acc parallel vector_length(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;


#pragma acc kernels num_gangs(num_gangs_k) /* { dg-error "'num_gangs_k' (un|was not )declared" } */
  ;
#pragma acc kernels num_workers(num_workers_k) /* { dg-error "'num_workers_k' (un|was not )declared" } */
  ;
#pragma acc kernels vector_length(vector_length_k) /* { dg-error "'vector_length_k' (un|was not )declared" } */
  ;

#pragma acc parallel num_gangs(num_gangs_p) /* { dg-error "'num_gangs_p' (un|was not )declared" } */
  ;
#pragma acc parallel num_workers(num_workers_p) /* { dg-error "'num_workers_p' (un|was not )declared" } */
  ;
#pragma acc parallel vector_length(vector_length_p) /* { dg-error "'vector_length_p' (un|was not )declared" } */
  ;


#pragma acc kernels num_gangs(f) /* { dg-error "'num_gangs' expression must be integral" } */
  ;
#pragma acc kernels num_workers(f) /* { dg-error "'num_workers' expression must be integral" } */
  ;
#pragma acc kernels vector_length(f) /* { dg-error "'vector_length' expression must be integral" } */
  ;

#pragma acc parallel num_gangs(f) /* { dg-error "'num_gangs' expression must be integral" } */
  ;
#pragma acc parallel num_workers(f) /* { dg-error "'num_workers' expression must be integral" } */
  ;
#pragma acc parallel vector_length(f) /* { dg-error "'vector_length' expression must be integral" } */
  ;


#pragma acc kernels num_gangs((float) 1) /* { dg-error "'num_gangs' expression must be integral" } */
  ;
#pragma acc kernels num_workers((float) 1) /* { dg-error "'num_workers' expression must be integral" } */
  ;
#pragma acc kernels vector_length((float) 1) /* { dg-error "'vector_length' expression must be integral" } */
  ;

#pragma acc parallel num_gangs((float) 1) /* { dg-error "'num_gangs' expression must be integral" } */
  ;
#pragma acc parallel num_workers((float) 1) /* { dg-error "'num_workers' expression must be integral" } */
  ;
#pragma acc parallel vector_length((float) 1) /* { dg-error "'vector_length' expression must be integral" } */
  ;


#pragma acc kernels num_gangs(0) /* { dg-warning "'num_gangs' value must be positive" } */
  ;
#pragma acc kernels num_workers(0) /* { dg-warning "'num_workers' value must be positive" } */
  ;
#pragma acc kernels vector_length(0) /* { dg-warning "'vector_length' value must be positive" } */
  ;

#pragma acc parallel num_gangs(0) /* { dg-warning "'num_gangs' value must be positive" } */
  ;
#pragma acc parallel num_workers(0) /* { dg-warning "'num_workers' value must be positive" } */
  ;
#pragma acc parallel vector_length(0) /* { dg-warning "'vector_length' value must be positive" } */
  ;


#pragma acc kernels num_gangs((int) -1.2) /* { dg-warning "'num_gangs' value must be positive" } */
  ;
#pragma acc kernels num_workers((int) -1.2) /* { dg-warning "'num_workers' value must be positive" } */
  ;
#pragma acc kernels vector_length((int) -1.2) /* { dg-warning "'vector_length' value must be positive" } */
  ;

#pragma acc parallel num_gangs((int) -1.2) /* { dg-warning "'num_gangs' value must be positive" } */
  ;
#pragma acc parallel num_workers((int) -1.2) /* { dg-warning "'num_workers' value must be positive" } */
  ;
#pragma acc parallel vector_length((int) -1.2) /* { dg-warning "'vector_length' value must be positive" } */
  ;


#pragma acc kernels \
  num_gangs(1) /* { dg-error "too many 'num_gangs' clauses" "" { target c } } */ \
  num_workers(1) /* { dg-error "too many 'num_workers' clauses" "" { target c } } */ \
  vector_length(1) /* { dg-error "too many 'vector_length' clauses" "" { target c } } */ \
  num_workers(1) /* { dg-error "too many 'num_workers' clauses" "" { target c++ } } */ \
  vector_length(1) /* { dg-error "too many 'vector_length' clauses" "" { target c++ } } */ \
  num_gangs(1) /* { dg-error "too many 'num_gangs' clauses" "" { target c++ } } */
  ;

#pragma acc parallel							\
  num_gangs(1) /* { dg-error "too many 'num_gangs' clauses" "" { target c } } */ \
  num_workers(1) /* { dg-error "too many 'num_workers' clauses" "" { target c } } */ \
  vector_length(1) /* { dg-error "too many 'vector_length' clauses" "" { target c } } */ \
  num_workers(1) /* { dg-error "too many 'num_workers' clauses" "" { target c++ } } */ \
  vector_length(1) /* { dg-error "too many 'vector_length' clauses" "" { target c++ } } */ \
  num_gangs(1) /* { dg-error "too many 'num_gangs' clauses" "" { target c++ } } */
  ;


#pragma acc kernels \
  num_gangs(-1) /* { dg-warning "'num_gangs' value must be positive" } */ \
  num_workers() /* { dg-error "expected (primary-|)expression before '\\)' token" } */ \
  vector_length(abc_k) /* { dg-error "'abc_k' (un|was not )declared" } */ \
  num_workers(0.5) /* { dg-error "'num_workers' expression must be integral" } */ \
  vector_length(&f) /* { dg-error "'vector_length' expression must be integral" } */ \
  num_gangs( /* { dg-error "expected (primary-|)expression before end of line" "TODO" { xfail c } } */
  ;

#pragma acc parallel							\
  num_gangs(-1) /* { dg-warning "'num_gangs' value must be positive" } */ \
  num_workers() /* { dg-error "expected (primary-|)expression before '\\)' token" } */ \
  vector_length(abc_p) /* { dg-error "'abc_p' (un|was not )declared" } */ \
  num_workers(0.5) /* { dg-error "'num_workers' expression must be integral" } */ \
  vector_length(&f) /* { dg-error "'vector_length' expression must be integral" } */ \
  num_gangs( /* { dg-error "expected (primary-|)expression before end of line" "TODO" { xfail c } } */
  ;


  /* The 'serial' construct doesn't allow these at all.  */

#pragma acc serial num_gangs (1) /* { dg-error "'num_gangs' is not valid for '#pragma acc serial'" } */
  ;
#pragma acc serial num_workers (1) /* { dg-error "'num_workers' is not valid for '#pragma acc serial'" } */
  ;
#pragma acc serial vector_length (1) /* { dg-error "'vector_length' is not valid for '#pragma acc serial'" } */
  ;
}
