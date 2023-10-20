/* See also 'self-clause-1.c'.  */

void
f (void)
{
  struct { int i; } *p;
#pragma acc data copyout(p) if(1) if(1) /* { dg-error "too many 'if' clauses" } */
  ;
#pragma acc update device(p) if(*p)
  /* { dg-error {used struct type value where scalar is required} {} { target c } .-1 }
     { dg-error {could not convert '\* p' from 'f\(\)::<unnamed struct>' to 'bool'} {} { target c++ } .-2 } */
}
