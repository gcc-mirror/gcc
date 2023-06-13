/* { dg-skip-if "not yet" { c++ } } */

void
f (int b)
{
  struct { int i; } *p;

#pragma acc parallel self self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc parallel self(*p) /* { dg-error "used struct type value where scalar is required" } */
  ;

#pragma acc kernels self self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc kernels self(*p) /* { dg-error "used struct type value where scalar is required" } */
  ;

#pragma acc serial self self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc serial self(*p) /* { dg-error "used struct type value where scalar is required" } */
  ;
}
