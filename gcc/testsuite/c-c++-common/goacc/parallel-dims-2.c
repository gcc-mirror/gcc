/* Invalid use of OpenACC parallelism dimensions clauses: num_gangs,
   num_workers, vector_length.  */

void acc_kernels(int i)
{
#pragma acc kernels num_gangs(i) /* { dg-error "'num_gangs' is not valid for '#pragma acc kernels'" } */
  ;
#pragma acc kernels num_workers(i) /* { dg-error "'num_workers' is not valid for '#pragma acc kernels'" } */
  ;
#pragma acc kernels vector_length(i) /* { dg-error "'vector_length' is not valid for '#pragma acc kernels'" } */
  ;
}

void acc_parallel(int i, float f)
{
#pragma acc parallel num_gangs /* { dg-error "expected '\\(' before end of line" } */
  ;
#pragma acc parallel num_workers /* { dg-error "expected '\\(' before end of line" } */
  ;
#pragma acc parallel vector_length /* { dg-error "expected '\\(' before end of line" } */
  ;

#pragma acc parallel num_gangs( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;
#pragma acc parallel num_workers( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;
#pragma acc parallel vector_length( /* { dg-error "expected (primary-|)expression before end of line" } */
  ;

#pragma acc parallel num_gangs() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;
#pragma acc parallel num_workers() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;
#pragma acc parallel vector_length() /* { dg-error "expected (primary-|)expression before '\\)' token" } */
  ;

#pragma acc parallel num_gangs(1 /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc parallel num_workers(1 /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc parallel vector_length(1 /* { dg-error "expected '\\)' before end of line" } */
  ;

#pragma acc parallel num_gangs(i /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc parallel num_workers(i /* { dg-error "expected '\\)' before end of line" } */
  ;
#pragma acc parallel vector_length(i /* { dg-error "expected '\\)' before end of line" } */
  ;

#pragma acc parallel num_gangs(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc parallel num_workers(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc parallel vector_length(1 i /* { dg-error "expected '\\)' before 'i'" } */
  ;

#pragma acc parallel num_gangs(1 i) /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc parallel num_workers(1 i) /* { dg-error "expected '\\)' before 'i'" } */
  ;
#pragma acc parallel vector_length(1 i) /* { dg-error "expected '\\)' before 'i'" } */
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

#pragma acc parallel num_gangs(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;
#pragma acc parallel num_workers(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;
#pragma acc parallel vector_length(1, i) /* { dg-error "expected '\\)' before ',' token" "TODO" { xfail c } } */
  ;

#pragma acc parallel num_gangs(num_gangs) /* { dg-error "'num_gangs' (un|was not )declared" } */
  ;
#pragma acc parallel num_workers(num_workers) /* { dg-error "'num_workers' (un|was not )declared" } */
  ;
#pragma acc parallel vector_length(vector_length) /* { dg-error "'vector_length' (un|was not )declared" } */
  ;

#pragma acc parallel num_gangs(f) /* { dg-error "'num_gangs' expression must be integral" } */
  ;
#pragma acc parallel num_workers(f) /* { dg-error "'num_workers' expression must be integral" } */
  ;
#pragma acc parallel vector_length(f) /* { dg-error "'vector_length' expression must be integral" } */
  ;

#pragma acc parallel num_gangs((float) 1) /* { dg-error "'num_gangs' expression must be integral" } */
  ;
#pragma acc parallel num_workers((float) 1) /* { dg-error "'num_workers' expression must be integral" } */
  ;
#pragma acc parallel vector_length((float) 1) /* { dg-error "'vector_length' expression must be integral" } */
  ;

#pragma acc parallel num_gangs(0) /* { dg-warning "'num_gangs' value must be positive" } */
  ;
#pragma acc parallel num_workers(0) /* { dg-warning "'num_workers' value must be positive" } */
  ;
#pragma acc parallel vector_length(0) /* { dg-warning "'vector_length' value must be positive" } */
  ;

#pragma acc parallel num_gangs((int) -1.2) /* { dg-warning "'num_gangs' value must be positive" } */
  ;
#pragma acc parallel num_workers((int) -1.2) /* { dg-warning "'num_workers' value must be positive" } */
  ;
#pragma acc parallel vector_length((int) -1.2) /* { dg-warning "'vector_length' value must be positive" } */
  ;

#pragma acc parallel \
  num_gangs(1) /* { dg-error "too many 'num_gangs' clauses" "" { target c } } */ \
  num_workers(1) /* { dg-error "too many 'num_workers' clauses" "" { target c } } */ \
  vector_length(1) /* { dg-error "too many 'vector_length' clauses" "" { target c } } */ \
  num_workers(1) /* { dg-error "too many 'num_workers' clauses" "" { target c++ } } */ \
  vector_length(1) /* { dg-error "too many 'vector_length' clauses" "" { target c++ } } */ \
  num_gangs(1) /* { dg-error "too many 'num_gangs' clauses" "" { target c++ } } */
  ;

#pragma acc parallel \
  num_gangs(-1) /* { dg-warning "'num_gangs' value must be positive" } */ \
  num_workers() /* { dg-error "expected (primary-|)expression before '\\)' token" } */ \
  vector_length(abc) /* { dg-error "'abc' (un|was not )declared" } */ \
  num_workers(0.5) /* { dg-error "'num_workers' expression must be integral" } */ \
  vector_length(&acc_parallel) /* { dg-error "'vector_length' expression must be integral" } */ \
  num_gangs( /* { dg-error "expected (primary-|)expression before end of line" "TODO" { xfail c } } */
  ;
}
