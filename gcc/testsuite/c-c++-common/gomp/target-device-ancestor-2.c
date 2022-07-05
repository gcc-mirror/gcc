/* { dg-do compile } */

#pragma omp requires reverse_offload

void
foo (int n)
{
  #pragma omp target device (ancestor: 1)
  ;


  /* Ensure that the integer expression in the 'device' clause for
     device-modifier 'ancestor' evaluates to '1' in case of a constant.  */

  #pragma omp target device (ancestor : 1)
  ;
  #pragma omp target device (ancestor : 42) /* { dg-error "the 'device' clause expression must evaluate to '1'" } */
  ;

  #pragma omp target device (ancestor : n)
  ;
  #pragma omp target device (ancestor : n + 1)
  ;


  /* Ensure that only one 'device' clause appears on the construct.  */

  #pragma omp target device (17) device (42) /* { dg-error "too many 'device' clauses" } */
  ;


  /* Ensure that with 'ancestor' only the 'device', 'firstprivate', 'private',
     'defaultmap', and 'map' clauses appear on the construct.  */

  #pragma omp target nowait device (ancestor: 1) /* { dg-error "with 'ancestor', only the 'device', 'firstprivate', 'private', 'defaultmap', and 'map' clauses may appear on the construct" } */
  ;
  #pragma omp target device (ancestor: 1) nowait /* { dg-error "with 'ancestor', only the 'device', 'firstprivate', 'private', 'defaultmap', and 'map' clauses may appear on the construct" } */
  ;
  #pragma omp target nowait device (42)
  ;
  #pragma omp target nowait device (device_num: 42)
  ;

  int a = 0, b = 0, c = 0;
  #pragma omp target device (ancestor: 1) firstprivate (a) private (b) defaultmap (none) map (c)
  ;


  /* Ensure that 'ancestor' is only used with 'target' constructs (not with
     'target data', 'target update' etc.).  */

  #pragma omp target data map (a) device (ancestor: 1) /* { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" } */
  ;
  #pragma omp target enter data map (to: a) device (ancestor: 1) /* { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" } */
  #pragma omp target exit data map (from: a) device (ancestor: 1) /* { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" } */
  #pragma omp target update to (a) device (ancestor: 1) /* { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" "" { target *-*-* } } */


  /* Ensure that no OpenMP constructs appear inside target regions with 
     'ancestor'.  */

  #pragma omp target device (ancestor: 1)
    {
      #pragma omp teams /* { dg-error "OpenMP constructs are not allowed in target region with 'ancestor'" } */
      ;
    }

  #pragma omp target device (device_num: 1) 
    {
      #pragma omp teams
      ;
    }

  #pragma omp target device (1) 
    {
      #pragma omp teams
      ;
    }

}
