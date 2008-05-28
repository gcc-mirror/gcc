! { dg-do run }
!
! This tests the (partial) fix for PR35830, i.e. handling array arguments
! with the PROCEDURE statement.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m
contains
  subroutine one(a)
      integer a(1:3)
      if (any(a /= [1,2,3])) call abort()
  end subroutine one
end module m

program test
  use m
  implicit none
  call foo(one)
contains
  subroutine foo(f)
    procedure(one) :: f
    call f([1,2,3])
  end subroutine foo
end program test

! { dg-final { cleanup-modules "m" } }
