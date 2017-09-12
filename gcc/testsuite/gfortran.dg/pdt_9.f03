! { dg-do compile }
!
! Test the fix for PR82168 in which the declarations for 'a'
! and 'b' threw errors even though they are valid.
!
! Contributed by  <physiker@toast2.net>
!
module mod
  implicit none
  integer, parameter :: dp = kind (0.0d0)
  type, public :: v(z, k)
    integer, len :: z
    integer, kind :: k = kind(0.0)
    real(kind = k) :: e(z)
  end type v
end module mod

program bug
  use mod
  implicit none
  type (v(2)) :: a     ! Missing parameter replaced by initializer.
  type (v(z=:, k=dp)), allocatable :: b ! Keyword was not working for '*' or ':'
end program bug
