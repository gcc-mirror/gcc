! { dg-do run }
!
! PR fortran/39427
!
! Check constructor functionality.
!
!
module m
  interface cons
    procedure cons42
  end interface cons
contains
  integer function cons42()
    cons42 = 42
  end function cons42
end module m


module m2
  type cons
    integer :: j = -1
  end type cons
  interface cons
    procedure consT
  end interface cons
contains
  type(cons) function consT(k)
    integer :: k
    consT%j = k**2
  end function consT
end module m2


use m
use m2, only: cons
implicit none
type(cons) :: x
integer :: k
x = cons(3)
k = cons()
if (x%j /= 9) call abort ()
if (k /= 42) call abort ()
!print *, x%j
!print *, k
end
