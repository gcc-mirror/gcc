! { dg-do "compile" }
! { dg-options "-fwhole-file" }
!
! PR fortran/31346
!
program main
  real, dimension(2) :: a
  call foo(a)                ! { dg-error "must have an explicit interface" }
end program main

subroutine foo(a)
  real, dimension(:) :: a
end subroutine foo
