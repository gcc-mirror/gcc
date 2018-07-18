/* OpenACC default clause: invalid syntax.  */

void f1 ()
{
#pragma acc kernels default /* { dg-error "expected .\\(. before end of line" } */
  ;
#pragma acc parallel default /* { dg-error "expected .\\(. before end of line" } */
  ;

#pragma acc kernels default ( /* { dg-error "expected .none. or .present. before end of line" } */
  ;
#pragma acc parallel default ( /* { dg-error "expected .none. or .present. before end of line" } */
  ;

#pragma acc kernels default (, /* { dg-error "expected .none. or .present. before .,. token" } */
  ;
#pragma acc parallel default (, /* { dg-error "expected .none. or .present. before .,. token" } */
  ;

#pragma acc kernels default () /* { dg-error "expected .none. or .present. before .\\). token" } */
  ;
#pragma acc parallel default () /* { dg-error "expected .none. or .present. before .\\). token" } */
  ;

#pragma acc kernels default (,) /* { dg-error "expected .none. or .present. before .,. token" } */
  ;
#pragma acc parallel default (,) /* { dg-error "expected .none. or .present. before .,. token" } */
  ;

#pragma acc kernels default (firstprivate) /* { dg-error "expected .none. or .present. before .firstprivate." } */
  ;
#pragma acc parallel default (firstprivate) /* { dg-error "expected .none. or .present. before .firstprivate." } */
  ;

#pragma acc kernels default (private) /* { dg-error "expected .none. or .present. before .private." } */
  ;
#pragma acc parallel default (private) /* { dg-error "expected .none. or .present. before .private." } */
  ;

#pragma acc kernels default (shared) /* { dg-error "expected .none. or .present. before .shared." } */
  ;
#pragma acc parallel default (shared) /* { dg-error "expected .none. or .present. before .shared." } */
  ;

#pragma acc kernels default (none /* { dg-error "expected .\\). before end of line" } */
  ;
#pragma acc parallel default (none /* { dg-error "expected .\\). before end of line" } */
  ;

#pragma acc kernels default (none none) /* { dg-error "expected .\\). before .none." } */
  ;
#pragma acc parallel default (none none) /* { dg-error "expected .\\). before .none." } */
  ;

#pragma acc kernels default (none, none) /* { dg-error "expected .\\). before .,. token" } */
  ;
#pragma acc parallel default (none, none) /* { dg-error "expected .\\). before .,. token" } */
  ;
}
