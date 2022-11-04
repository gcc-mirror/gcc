! { dg-do run }
! PR fortran/93483
!
! Verify that resolution (host associated parameter vs. contained function) works.
!
! Contributed by Mikael Morin

module m
  implicit none
  integer, parameter :: a(*) = [ 7, 11 ]
contains
  subroutine bug
    real :: b(1), c(1)
    b = [ real :: (a(1)) ]
    c = [ real ::  a(1)  ]
    print *, b, c
    if (any (b /= [ 14. ])) stop 1
    if (any (c /= [ 14. ])) stop 2
  contains
    function a(c)
      integer :: a, c
      a = c + 13
    end function a
  end subroutine bug
end module m

program p
  use m
  call bug
end program p
