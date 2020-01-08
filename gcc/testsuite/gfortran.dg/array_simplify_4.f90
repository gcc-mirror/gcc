! { dg-do compile }
!
! PR fortran/92996
!
! Contributed by G. Steinmetz
!

module m
  integer, parameter :: d(2) = [0,0]
end module m

subroutine one
use m
print size([1,2],dim=d(1)) ! { dg-error "'dim' argument of 'size' intrinsic at .1. is not a valid dimension index" }
end

subroutine two
complex, parameter :: x = 1

stop x  ! { dg-error "STOP code at .1. must be either INTEGER or CHARACTER type" }
end

program p
   integer, parameter :: a(2) = [1, 2]
   stop a(1) ! OK
   stop a ! { dg-error "STOP code at .1. must be scalar" }
   stop a(1,1) ! { dg-error "Rank mismatch in array reference at .1. .2/1." }
end
