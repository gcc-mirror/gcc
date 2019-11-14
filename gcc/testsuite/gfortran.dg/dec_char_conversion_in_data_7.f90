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

  data a / 4_'1234' /     ! { dg-error "attempted conversion of CHARACTER\\(4,4\\)" }
  data b / 4_'12'   /     ! { dg-error "attempted conversion of CHARACTER\\(2,4\\)" }
  data c / 4_'12341234' / ! { dg-error "attempted conversion of CHARACTER\\(8,4\\)" }
  data d / 4_'abcd' /     ! { dg-error "attempted conversion of CHARACTER\\(4,4\\)" }
end program

