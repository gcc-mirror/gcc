/* See also 'if-clause-1.c'.  */

void
f (int b)
{
  struct { int i; } *p;

#pragma acc parallel self(0) self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc parallel self self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc parallel self(*p)
  /* { dg-error {used struct type value where scalar is required} {} { target c } .-1 }
     { dg-error {could not convert '\* p' from 'f\(int\)::<unnamed struct>' to 'bool'} {} { target c++ } .-2 } */
  ;

#pragma acc kernels self(0) self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc kernels self self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc kernels self(*p)
  /* { dg-error {used struct type value where scalar is required} {} { target c } .-1 }
     { dg-error {could not convert '\* p' from 'f\(int\)::<unnamed struct>' to 'bool'} {} { target c++ } .-2 } */
  ;

#pragma acc serial self(0) self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc serial self self(b) /* { dg-error "too many 'self' clauses" } */
  ;
#pragma acc serial self(*p)
  /* { dg-error {used struct type value where scalar is required} {} { target c } .-1 }
     { dg-error {could not convert '\* p' from 'f\(int\)::<unnamed struct>' to 'bool'} {} { target c++ } .-2 } */
  ;
}
