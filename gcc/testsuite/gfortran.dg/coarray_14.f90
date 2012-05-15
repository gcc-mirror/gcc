! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/46370
!
! Coarray checks
!

! Check for C1229: "A data-ref shall not be a polymorphic subobject of a
! coindexed object." which applies to function and subroutine calls.
module m
  implicit none
  type t
  contains
    procedure, nopass :: sub=>sub
    procedure, nopass :: func=>func
  end type t
  type t3
    type(t) :: nopoly
  end type t3
  type t2
    class(t), allocatable :: poly
    class(t3), allocatable :: poly2
  end type t2
contains
  subroutine sub()
  end subroutine sub
  function func()
    integer :: func
  end function func
end module m

subroutine test(x)
  use m
  type(t2) :: x[*]
  integer :: i
  call x[1]%poly2%nopoly%sub() ! OK
  i = x[1]%poly2%nopoly%func() ! OK
  call x[1]%poly%sub() ! { dg-error "Polymorphic subobject of coindexed object" }
  i = x[1]%poly%func() ! { dg-error "Polymorphic subobject of coindexed object" }
end subroutine test


! Check for C617: "... a data-ref shall not be a polymorphic subobject of a
! coindexed object or ..." 
! Before, the second allocate statment was failing - though it is no subobject.
program myTest
type t
end type t
type(t), allocatable :: a[:]
 allocate (t :: a) ! { dg-error "Coarray specification required in ALLOCATE statement" }
allocate (t :: a[*]) ! OK
end program myTest
