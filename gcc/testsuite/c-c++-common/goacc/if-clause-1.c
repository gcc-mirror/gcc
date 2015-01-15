/* { dg-skip-if "not yet" { c++ } } */

void
f (void)
{
  struct { int i; } *p;
#pragma acc data copyout(p) if(1) if(1) /* { dg-error "too many 'if' clauses" } */
  ;
#pragma acc update device(p) if(*p) /* { dg-error "used struct type value where scalar is required" } */
}
