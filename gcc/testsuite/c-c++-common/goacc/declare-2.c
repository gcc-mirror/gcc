/* Test invalid use of the OpenACC 'declare' directive.  */


#pragma acc declare /* { dg-error "no valid clauses" } */

#pragma acc declare create(undeclared) /* { dg-error "undeclared" } */
/* { dg-error "no valid clauses" "second error" { target *-*-* } .-1 } */

int v0[10];
#pragma acc declare create(v0[1:3]) /* { dg-error "array section" } */

int v1;
#pragma acc declare create(v1, v1) /* { dg-error "more than once" } */

int v2;
#pragma acc declare create(v2)
#pragma acc declare copyin(v2) /* { dg-error "more than once" } */

int v3;
#pragma acc declare copy(v3) /* { dg-error "at file scope" } */

int v4;
#pragma acc declare copyout(v4) /* { dg-error "at file scope" } */

int v5;
#pragma acc declare present(v5) /* { dg-error "at file scope" } */

int v6;
#pragma acc declare present_or_copy(v6) /* { dg-error "at file scope" } */

int v7;
#pragma acc declare present_or_copyout(v7) /* { dg-error "at file scope" } */

int va10;
#pragma acc declare create (va10)
#pragma acc declare link (va10) /* { dg-error "more than once" } */

int va11;
#pragma acc declare link (va11)
#pragma acc declare link (va11) /* { dg-error "more than once" } */

int va12;
#pragma acc declare create (va12) link (va12) /* { dg-error "more than once" } */


void
f (void)
{
  int va0;
#pragma acc declare link(va0) /* { dg-error "global variable" } */

  extern int ve0;
#pragma acc declare copy(ve0) /* { dg-error "invalid use of" } */

  extern int ve1;
#pragma acc declare copyout(ve1) /* { dg-error "invalid use of" } */

  extern int ve2;
#pragma acc declare present(ve2) /* { dg-error "invalid use of" } */

  extern int ve3;
#pragma acc declare present_or_copy(ve3) /* { dg-error "invalid use of" } */

  extern int ve4;
#pragma acc declare present_or_copyout(ve4) /* { dg-error "invalid use of" } */

#pragma acc declare present (v2) /* { dg-error "invalid use of" } */
}


/* The same as 'f' but everything contained in an OpenACC 'data' construct.  */

void
f_data (void)
{
#pragma acc data
  {
    int va0;
# pragma acc declare link(va0) /* { dg-error "global variable" } */

    extern int ve0;
# pragma acc declare copy(ve0) /* { dg-error "invalid use of" } */

    extern int ve1;
# pragma acc declare copyout(ve1) /* { dg-error "invalid use of" } */

    extern int ve2;
# pragma acc declare present(ve2) /* { dg-error "invalid use of" } */

    extern int ve3;
# pragma acc declare present_or_copy(ve3) /* { dg-error "invalid use of" } */

    extern int ve4;
# pragma acc declare present_or_copyout(ve4) /* { dg-error "invalid use of" } */

# pragma acc declare present (v2) /* { dg-error "invalid use of" } */
  }
}


/* Testing for PR90868 "Duplicate OpenACC 'declare' directives for 'extern'
   variables".  */


void
f_pr90868 (void)
{
  extern int we0;
#pragma acc declare create(we0)

  extern int we1;
#pragma acc declare copyin(we1)

  extern int *we2;
#pragma acc declare deviceptr(we2)

  extern int we3;
#pragma acc declare device_resident(we3)

  extern int we4;
#pragma acc declare link(we4)

  extern int we5;
#pragma acc declare present_or_copyin(we5)
 
  extern int we6;
#pragma acc declare present_or_create(we6)
}


/* The same as 'f_pr90868'.  */

/* The errors are emitted for C only; for C++, the duplicate OpenACC 'declare'
   directives for 'extern' variables are accepted.  */

void
f_pr90868_2 (void)
{
  extern int we0;
#pragma acc declare create(we0) /* { dg-error "variable 'we0' used more than once with '#pragma acc declare'" "" { target c } } */

  extern int we1;
#pragma acc declare copyin(we1) /* { dg-error "variable 'we1' used more than once with '#pragma acc declare'" "" { target c } } */

  extern int *we2;
#pragma acc declare deviceptr(we2) /* { dg-error "variable 'we2' used more than once with '#pragma acc declare'" "" { target c } } */

  extern int we3;
#pragma acc declare device_resident(we3) /* { dg-error "variable 'we3' used more than once with '#pragma acc declare'" "" { target c } } */

  extern int we4;
#pragma acc declare link(we4) /* { dg-error "variable 'we4' used more than once with '#pragma acc declare'" "" { target c } } */

  extern int we5;
#pragma acc declare present_or_copyin(we5) /* { dg-error "variable 'we5' used more than once with '#pragma acc declare'" "" { target c } } */
 
  extern int we6;
#pragma acc declare present_or_create(we6) /* { dg-error "variable 'we6' used more than once with '#pragma acc declare'" "" { target c } } */
}


/* The same as 'f_pr90868' but everything contained in an OpenACC 'data'
   construct.  */

#ifdef __cplusplus
/* TODO PR90868

   C++: ICE during gimplification.  */
#else
void
f_pr90868_data (void)
{
#pragma acc data
  {
    extern int we0;
# pragma acc declare create(we0) /* { dg-error "variable 'we0' used more than once with '#pragma acc declare'" "" { target c } } */

    extern int we1;
# pragma acc declare copyin(we1) /* { dg-error "variable 'we1' used more than once with '#pragma acc declare'" "" { target c } } */

    extern int *we2;
# pragma acc declare deviceptr(we2) /* { dg-error "variable 'we2' used more than once with '#pragma acc declare'" "" { target c } } */

    extern int we3;
# pragma acc declare device_resident(we3) /* { dg-error "variable 'we3' used more than once with '#pragma acc declare'" "" { target c } } */

    extern int we4;
# pragma acc declare link(we4) /* { dg-error "variable 'we4' used more than once with '#pragma acc declare'" "" { target c } } */

    extern int we5;
# pragma acc declare present_or_copyin(we5) /* { dg-error "variable 'we5' used more than once with '#pragma acc declare'" "" { target c } } */
 
    extern int we6;
# pragma acc declare present_or_create(we6) /* { dg-error "variable 'we6' used more than once with '#pragma acc declare'" "" { target c } } */
  }
}
#endif
