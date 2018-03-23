! { dg-do run }
! Tests the fix for PR31229, PR31154 and PR33334, in which
! the KIND and TYPE parameters in the function declarations
! would cause errors.
!
! Contributed by Brooks Moses <brooks@gcc.gnu.org>
!           and Tobias Burnus <burnus@gcc.gnu.org>
!
module kinds
  implicit none
  integer, parameter :: dp = selected_real_kind(6)
  type t
     integer :: i
  end type t
  interface
    real(dp) function y()
      import
    end function
  end interface
end module kinds

type(t) function func() ! The legal bit of PR33334
  use kinds
  func%i = 5
end function func

real(dp) function another_dp_before_defined ()
  use kinds
  another_dp_before_defined = real (kind (4.0_DP))
end function

module mymodule;
contains
  REAL(2*DP) function declared_dp_before_defined()
    use kinds, only: dp
    real (dp) :: x
    declared_dp_before_defined = 1.0_dp
    x = 1.0_dp
    declared_dp_before_defined = real (kind (x))
  end function
end module mymodule

  use kinds
  use mymodule
  type(t), external :: func
  type(t) :: z
  if (kind (y ()) .ne. 4) STOP 1
  if (kind (declared_dp_before_defined ()) .ne. 8) STOP 2
  if (int (declared_dp_before_defined ()) .ne. 4) STOP 3
  if (int (another_dp_before_defined ()) .ne. 4) STOP 4
  z = func()
  if (z%i .ne. 5) STOP 5
end
