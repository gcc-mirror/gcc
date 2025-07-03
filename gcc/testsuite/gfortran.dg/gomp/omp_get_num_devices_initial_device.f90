! { dg-do compile }
! { dg-additional-options "-O1 -fdump-tree-optimized" }
integer function f() result(ret)
  interface
    integer function omp_get_initial_device (); end
    integer function omp_get_num_devices (); end
  end interface

  if (omp_get_initial_device () /= omp_get_num_devices ()) error stop

  if (omp_get_num_devices () /= omp_get_num_devices ()) error stop

  if (omp_get_initial_device () /= omp_get_initial_device ()) error stop

  ret = omp_get_num_devices ()
end

! { dg-final { scan-tree-dump-not "error_stop" "optimized" } }

! { dg-final { scan-tree-dump-not "omp_get_num_devices" "optimized" { target { ! offloading_enabled } } } }
! { dg-final { scan-tree-dump "return 0;" "optimized" { target { ! offloading_enabled } } } }

! { dg-final { scan-tree-dump-times "omp_get_num_devices" 1 "optimized" { target offloading_enabled } } }
! { dg-final { scan-tree-dump "_1 = __builtin_omp_get_num_devices \\(\\);\[\\r\\n\]+\[ \]+return _1;" "optimized" { target offloading_enabled } } }
