#ifdef __cplusplus
extern "C" {
#endif

int omp_get_num_teams (void);

#ifdef __cplusplus
}
#endif

/* { dg-do compile } */

#pragma omp requires reverse_offload /* { dg-message "sorry, unimplemented: 'reverse_offload' clause on 'requires' directive not supported yet" } */

void
foo (void)
{
  /* Ensure that no calls to OpenMP API runtime routines are allowed inside the
     corresponding target region.  */

  int a;

  #pragma omp target device (ancestor: 1)
    {
      a = omp_get_num_teams (); /* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_teams\[^\n\r]*' in a region with 'device\\(ancestor\\)' clause" }  */
    }

  #pragma omp target device (device_num: 1)
    {
      a = omp_get_num_teams ();
    }

  #pragma omp target device (1)
    {
      a = omp_get_num_teams ();
    }
}
