/* { dg-do compile }  */
/* { dg-additional-options "-O1 -fdump-tree-optimized" }  */

#ifdef __cplusplus
extern "C" {
#endif
extern int omp_get_initial_device ();
extern int omp_get_num_devices ();
#ifdef __cplusplus
}
#endif

int f()
{
/* The following assumes that omp_get_initial_device () will not return
   omp_initial_device (== -1), which is also permitted since OpenMP 6.0.  */
  if (omp_get_initial_device () != omp_get_num_devices ()) __builtin_abort ();

  if (omp_get_num_devices () != omp_get_num_devices ()) __builtin_abort ();

  if (omp_get_initial_device () != omp_get_initial_device ()) __builtin_abort ();

  return omp_get_num_devices ();
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } }  */

/* { dg-final { scan-tree-dump-not "omp_get_num_devices" "optimized" { target { ! offloading_enabled } } } }  */
/* { dg-final { scan-tree-dump "return 0;" "optimized" { target { ! offloading_enabled } } } }  */

/* { dg-final { scan-tree-dump-times "omp_get_num_devices" 1 "optimized" { target offloading_enabled } } }  */
/* { dg-final { scan-tree-dump "_1 = __builtin_omp_get_num_devices \\(\\);\[\\r\\n\]+\[ \]+return _1;" "optimized" { target offloading_enabled } } }  */
