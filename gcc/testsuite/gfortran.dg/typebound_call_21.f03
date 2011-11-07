! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 50919: [OOP] Don't use vtable for NON_OVERRIDABLE TBP
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m

type t
contains
  procedure, nopass, NON_OVERRIDABLE :: testsub
  procedure, nopass, NON_OVERRIDABLE :: testfun
end type t

contains

  subroutine testsub()
    print *, "t's test"
  end subroutine

  integer function testfun()
    testfun = 1
  end function

end module m


  use m
  class(t), allocatable :: x
  allocate(x)
  call x%testsub()
  print *,x%testfun()
end

! { dg-final { scan-tree-dump-times "_vptr->" 0 "original" } }

! { dg-final { cleanup-modules "m" } }
! { dg-final { cleanup-tree-dump "original" } }
