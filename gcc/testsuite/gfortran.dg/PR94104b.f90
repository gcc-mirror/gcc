! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/94104
!

program diag_p
  implicit none

  integer, parameter :: n = 7

  integer         :: a(n)
  integer, target :: b(n)

  a = 1
  print *, sumf(a) ! { dg-error "Actual argument for 'a' at .1. must be a pointer or a valid target" }
  print *, sumf(b)

contains

  function sumf(a) result(s)
    integer, pointer, intent(in) :: a(:)

    integer :: s

    s = sum(a)
  end function sumf

end program diag_p
