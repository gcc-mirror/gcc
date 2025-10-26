! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR122290.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module hyperparameters_m
  implicit none

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    real(k) :: learning_rate_ = real(1.5,k)                       ! Gave "Invalid kind for REAL"
  contains
    generic :: operator(==) => default_real_equals, real8_equals  ! Gave "Entity ‘default_real_equals’ at (1)
                                                                  ! is already present in the interface"
    generic :: g => default_real_equals, real8_equals             ! Make sure that ordinary generic is OK
    procedure default_real_equals
    procedure real8_equals
  end type

  interface
    logical module function default_real_equals(lhs, rhs)
      implicit none
      class(hyperparameters_t), intent(in) :: lhs, rhs
    end function
    logical module function real8_equals(lhs, rhs)
      implicit none
      class(hyperparameters_t(kind(1d0))), intent(in) :: lhs, rhs
    end function
  end interface
end module

! Added to test generic procedures are the correct ones.
submodule(hyperparameters_m) hyperparameters_s
contains
    logical module function default_real_equals(lhs, rhs)
      implicit none
      class(hyperparameters_t), intent(in) :: lhs, rhs
      default_real_equals = (lhs%learning_rate_ == rhs%learning_rate_)
    end function
    logical module function real8_equals(lhs, rhs)
      implicit none
      class(hyperparameters_t(kind(1d0))), intent(in) :: lhs, rhs
      real8_equals = (lhs%learning_rate_ == rhs%learning_rate_)
    end function
end submodule

  use hyperparameters_m
  type (hyperparameters_t) :: a, b
  type (hyperparameters_t(kind(1d0))) :: c, d
  if (.not.(a == b)) stop 1
  if (.not.a%g(b)) stop 2
  a%learning_rate_ = real(2.5,a%k)
  if (a == b) stop 3
  if (a%g(b)) stop 4

  if (.not.(c == d)) stop 5
  if (.not.c%g(d)) stop 6
  c%learning_rate_ = real(2.5,c%k)
  if (c == d) stop 7
  if (c%g(d)) stop 8
end
! { dg-final { scan-tree-dump-times "default_real_equals" 8 "original" } }
! { dg-final { scan-tree-dump-times "real8_equals" 8 "original" } }
