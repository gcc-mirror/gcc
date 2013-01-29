! { dg-do compile }
! { dg-options "-fcoarray=single" }

! Requires that coarray_29.f90 has been compiled before
! and that, thus, co_sum_module is available

! PR fortran/55272
!
! Contributed by Damian Rouson

program main
  use co_sum_module
  implicit none    
  integer score[*] 
  call co_sum(score)
end program

! { dg-final { cleanup-modules "co_sum_module" } }
