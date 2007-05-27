! { dg-do compile }
! PR fortran/32088
!
! Test implicitly defined result variables
!
subroutine dummy
contains
  function quadric(a,b) result(c)
  intent(in) a,b; dimension a(0:3),b(0:3),c(0:9)
    c(0)=a(0)*b(0); c(1:3)=a(1:)*b(0)+a(0)*b(1:); c(4:6)=a(1:)*b(1:)
    c(7:9)=(/a(1)*b(2)+b(1)*a(2),a(1)*b(3)+b(1)*a(3),a(2)*b(3)+b(2)*a(3)/)
  end function
end subroutine dummy

subroutine dummy2
implicit none
contains
  function quadric(a,b) result(c) ! { dg-error "no IMPLICIT type" }
  real :: a, b
  intent(in) a,b; dimension a(0:3),b(0:3),c(0:9)
    c(0)=a(0)*b(0); c(1:3)=a(1:)*b(0)+a(0)*b(1:); c(4:6)=a(1:)*b(1:)
    c(7:9)=(/a(1)*b(2)+b(1)*a(2),a(1)*b(3)+b(1)*a(3),a(2)*b(3)+b(2)*a(3)/)
  end function
end subroutine dummy2
end
