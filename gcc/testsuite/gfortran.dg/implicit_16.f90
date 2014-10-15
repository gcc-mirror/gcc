! { dg-do compile }
! { dg-options "" }
!
! Support Fortran 2015's IMPLICIT NONE with empty spec list
!
! And IMPLICIT with ";" followed by an additional statement.
! Contributed by Alan Greynolds
!

module m
  type t
  end type t
end module m

subroutine sub0
implicit integer (a-h,o-z); parameter (i=0)
end subroutine sub0

subroutine sub1
implicit integer (a-h,o-z)!test
parameter (i=0)
end subroutine sub1

subroutine sub2
use m
implicit type(t) (a-h,o-z); parameter (i=0)
end subroutine sub2


subroutine sub3
use m
implicit type(t) (a-h,o-z)! Foobar
parameter (i=0)
end subroutine sub3

subroutine sub4
implicit none ()
call test()
i = 1 ! { dg-error "Symbol 'i' at .1. has no IMPLICIT type" }
end subroutine sub4
