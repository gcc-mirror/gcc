!==================initialization_1.f90======================

! { dg-do compile }
! Tests fix for PR25018 in which an ICE resulted from using a
! variable in a parameter initialization expression. In the course
! of developing the fix, various other constraints and limitations
! were tested.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module const
! The next line is the original error
  real(8), parameter :: g = - sqrt(2._8) * Gf ! { dg-error "not been declared or is a variable" }
contains
  subroutine foo(ch1, x, y)
    character(*)        :: ch1

! This is OK because it is a restricted expression.
    character(len(ch1)) :: ch2

    real(8) :: x (1:2, *)
    real(8) :: y (0:,:)
    integer :: i
    real :: z(2, 2)

! However, this gives a warning because it is an initialization expression.
    integer :: l1 = len (ch1)     ! { dg-error "Assumed or deferred character length variable" }

! These are warnings because they are gfortran extensions.
    integer :: m3 = size (x, 1)   ! { dg-error "Assumed size array" }
    integer :: m4(2) = shape (z)

! This does not depend on non-constant properties.
    real(8) :: big = huge (x)

  end subroutine foo  
end module const
