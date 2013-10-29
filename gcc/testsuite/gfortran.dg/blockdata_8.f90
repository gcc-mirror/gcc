! { dg-do compile }
!
! PR fortran/44350
!
! Fortran 2008, C1116 only permits a small subset of statements in BLOCK DATA
!
! Part of the test case was contributed by Vittorio Zecca
!
module m
end module m

BLOCK DATA valid2
  use m
  implicit integer(a-z)
  intrinsic :: sin
  common /one/ a, c
  bind(C) :: /one/
  dimension c(5)
  parameter (g = 7)
END BLOCK DATA valid2

BLOCK DATA valid
  use m
  implicit none
  type t
    sequence
  end type t
  type(t), save :: x
  integer  :: y
  real :: q
  save :: y
  dimension :: q(5)
!  class(*) :: zz ! See PR fortran/58857
!  pointer :: zz
  target :: q
  volatile y
  asynchronous q
END BLOCK DATA valid

block data invalid
      common x
      f(x)=x ! { dg-error "STATEMENT FUNCTION statement is not allowed inside of BLOCK DATA" }
      interface ! { dg-error "INTERFACE statement is not allowed inside of BLOCK DATA" }
      end interface
1     format() ! { dg-error "FORMAT statement is not allowed inside of BLOCK DATA" }
end block invalid ! { dg-error "Expecting END BLOCK DATA statement" }

! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
