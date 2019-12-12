! { dg-do compile }
! { dg-options "-fdec" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
program test
  integer(4) :: a
  real(4) :: b
  complex(4) :: c
  logical(4) :: d

  a = 4_'1234'     ! { dg-error "Cannot convert CHARACTER\\(4,4\\) to" }
  b = 4_'12'       ! { dg-error "Cannot convert CHARACTER\\(2,4\\) to" }
  c = 4_'12341234' ! { dg-error "Cannot convert CHARACTER\\(8,4\\) to" }
  d = 4_'abcd'     ! { dg-error "Cannot convert CHARACTER\\(4,4\\) to" }
end program

