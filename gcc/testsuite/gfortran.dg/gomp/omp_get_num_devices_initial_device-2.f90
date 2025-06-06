! { dg-do compile }
! { dg-additional-options "-O1 -fdump-tree-optimized -fno-builtin-omp_get_num_devices -fno-builtin-omp_get_initial_device" }
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

! { dg-final { scan-tree-dump-times "error_stop" 3 "optimized" } }

! { dg-final { scan-tree-dump-times "omp_get_num_devices" 4 "optimized" } }
! { dg-final { scan-tree-dump-times "omp_get_initial_device" 3 "optimized" } }
