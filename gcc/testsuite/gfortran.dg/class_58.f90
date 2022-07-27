! { dg-do compile }
!
! PR 68440: [OOP] ICE on declaring class variable with wrong attribute
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

subroutine s
  type t
  end type
  class(t), parameter :: x = t()  ! { dg-error "cannot have the PARAMETER attribute" }
  class(t), parameter :: y = x    ! { dg-error "cannot have the PARAMETER attribute" }
  class(t) :: z = t()             ! { dg-error "must be dummy, allocatable or pointer" }
end
