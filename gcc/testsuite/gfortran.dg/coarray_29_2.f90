! { dg-do compile }
! { dg-options "-fcoarray=single" }
! { dg-compile-aux-modules "coarray_29_1.f90" }

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
