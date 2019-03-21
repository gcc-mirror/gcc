! Invalid use of routines defined inside a Fortran module.

! { dg-compile-aux-modules "routine-module-mod-1.f90" }

program main
  use routine_module_mod_1
  implicit none

  integer :: i

  !$acc parallel loop gang
  do i = 1, 10
     call g_1 ! { dg-error "routine call uses same OpenACC parallelism as containing loop" }
  end do
  !$acc end parallel loop

  !$acc parallel loop worker
  do i = 1, 10
     call g_1 ! { dg-error "routine call uses same OpenACC parallelism as containing loop" }
     call w_1 ! { dg-error "routine call uses same OpenACC parallelism as containing loop" }
  end do
  !$acc end parallel loop

  !$acc parallel loop vector
  do i = 1, 10
     call g_1 ! { dg-error "routine call uses same OpenACC parallelism as containing loop" }
     call w_1 ! { dg-error "routine call uses same OpenACC parallelism as containing loop" }
     call v_1 ! { dg-error "routine call uses same OpenACC parallelism as containing loop" }
  end do
  !$acc end parallel loop
end program main
