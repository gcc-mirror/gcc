! { dg-do compile }

! This testcase ensure that no calls to OpenMP API runtime routines are allowed
! inside the corresponding target region.

module my_omp_mod
 use iso_c_binding
 interface
   integer function omp_get_thread_num ()
   end
 end interface
end

subroutine f1 ()
  use my_omp_mod
  implicit none
  integer :: n

  !$omp requires reverse_offload

  !$omp target device (ancestor : 1)
    n = omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call 'omp_get_thread_num' in a region with 'device\\(ancestor\\)' clause" }
  !$omp end target

  !$omp target device (device_num : 1)
    n = omp_get_thread_num ()
  !$omp end target

  !$omp target device (1)
    n = omp_get_thread_num ()
  !$omp end target

end
