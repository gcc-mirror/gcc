! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/94104
!

program diag_p
  implicit none

  integer, parameter :: n = 7

  integer         :: a(n)
  integer, target :: b(n)

  a = 1
  print *, sumf(a) ! { dg-error "Actual argument for 'a' at .1. must be a pointer" }
  print *, sumf(b) ! { dg-error "Fortran 2008: Non-pointer actual argument at .1. to pointer dummy 'a'" }

contains

  function sumf(a) result(s)
    integer, pointer, intent(in) :: a(:)

    integer :: s

    s = sum(a)
  end function sumf

end program diag_p
