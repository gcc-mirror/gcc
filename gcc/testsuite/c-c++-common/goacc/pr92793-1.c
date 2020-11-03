/* Verify column location information.  */

/* See also 'gfortran.dg/goacc/pr92793-1.f90'.  */

/* { dg-additional-options "-fdump-tree-original-lineno" }, and also
   { dg-additional-options "-fdump-tree-gimple-lineno" } as the former doesn't
   actually contain location information.  */

/* No tabs.  Funny indentation/spacing for a reason.  */


static void
check ()
{
  int i, j, sum, diff;

 #pragma   acc  parallel \
  /* C, C++ location information points to the 'a' in '#pragma acc parallel'.  */ \
  /* { dg-final { scan-tree-dump-times "pr92793-1\\\.c:17:12\\\] #pragma acc parallel" 1 "original" { xfail *-*-* } } } */ \
  /* { dg-final { scan-tree-dump-times "pr92793-1\\\.c:17:12\\\] #pragma omp target oacc_parallel" 1 "gimple" } } */
  {
#pragma     acc loop \
  /* C, C++ location information points to the 'a' in '#pragma acc loop'.  */ \
  /* { dg-final { scan-tree-dump-times "pr92793-1\\\.c:22:13\\\] #pragma acc loop" 1 "original" { xfail *-*-* } } } */ \
  /* { dg-final { scan-tree-dump-times "pr92793-1\\\.c:22:13\\\] #pragma acc loop" 1 "gimple" } } */ \
       reduction  (  + :   sum) /* { dg-line sum1 } */ \
  /* C location information points to the '(' in 'reduction(+:sum)'.  */ \
  /* { dg-message "19: location of the previous reduction for 'sum'" "" { target c } sum1 } */ \
  /* C++ location information points to 'sum' in 'reduction(+:sum)'.  */ \
  /* { dg-message "28: location of the previous reduction for 'sum'" "" { target c++ } sum1 } */ \
  independent
    for (i = 1; i <= 10; i++)
      {
     #pragma      acc     loop \
  /* C, C++ location information points to the 'a' in '#pragma acc loop'.  */ \
  /* { dg-final { scan-tree-dump-times "pr92793-1\\\.c:34:19\\\] #pragma acc loop" 1 "original" { xfail *-*-* } } } */ \
  /* { dg-final { scan-tree-dump-times "pr92793-1\\\.c:34:19\\\] #pragma acc loop" 1 "gimple" } } */ \
 reduction  ( -  : diff  ) \
reduction(-:sum  ) /* { dg-line sum2 } */ \
  /* C location information points to the '(' in 'reduction(-:sum)'.  */ \
  /* { dg-warning "10: conflicting reduction operations for 'sum'" "" { target c } sum2 } */ \
  /* C++ location information points to 'sum' in 'reduction(-:sum)'.  */ \
  /* { dg-warning "13: conflicting reduction operations for 'sum'" "" { target c++ } sum2 } */ \
  independent
        for (j = 1; j <= 10; j++)
          {
                sum
                  =
             1 ;
            /* C, C++ location information points to the '=' in 'sum = 1'.  */ \
            /* { dg-final { scan-tree-dump-times "pr92793-1\\\.c:48:19\\\] sum = 1" 1 "original" { xfail *-*-* } } } */ \
            /* { dg-final { scan-tree-dump-times "pr92793-1\\\.c:48:19\\\] sum = 1" 1 "gimple" } } */
          }
      }
  }
}


void
a_sl() {
#pragma acc serial loop /* { dg-message "9: enclosing parent compute construct" } */ \
    gang(num:5) /* { dg-error "5: argument not permitted on 'gang' clause" } */ \
  worker(num:5) /* { dg-error "3: argument not permitted on 'worker' clause" } */ \
   vector(length:5) /* { dg-error "4: argument not permitted on 'vector' clause" } */
  for (int i = 0; i < 10; i++)
    ;
}

void
a_s_l() {
#pragma acc serial /* { dg-message "9: enclosing parent compute construct" } */
  {
#pragma acc loop \
       gang(num:5) /* { dg-error "8: argument not permitted on 'gang' clause" } */ \
   worker(num:5) /* { dg-error "4: argument not permitted on 'worker' clause" } */ \
  vector(length:5) /* { dg-error "3: argument not permitted on 'vector' clause" } */
    for (int i = 0; i < 10; i++)
      ;
  }
}

void a_r();
#pragma acc routine(a_r)

void
a_r() { /* { dg-message "1: enclosing routine" } */
#pragma acc loop \
   gang(num:5) /* { dg-error "4: argument not permitted on 'gang' clause" } */ \
    worker(num:5) /* { dg-error "5: argument not permitted on 'worker' clause" } */ \
  vector(length:5) /* { dg-error "3: argument not permitted on 'vector' clause" } */
  for (int i = 0; i < 10; i++)
    ;
}
