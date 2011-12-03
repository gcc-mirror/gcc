! { dg-do compile }
!
! Test diagnostic for MOVE_ALLOC:
! FROM=type, TO=class is OK
! FROM=class, TO=type is INVALID
!
module m2
  type, abstract :: t2
  contains
    procedure(intf), deferred, nopass :: f
  end type t2

  interface
    function intf()
      import
      class(t2), allocatable :: intf
    end function intf
  end interface
end module m2

module m3
  use m2
  type, extends(t2) :: t3
  contains
    procedure,nopass :: f => my_f
  end type t3
contains
   function my_f()
     class(t2), allocatable :: my_f
   end function my_f
end module m3

subroutine my_test
use m3
type(t3), allocatable :: x
class(t2), allocatable :: y
call move_alloc (x, y)
end subroutine my_test

program testmv1
  type bar
  end type

  type, extends(bar) ::  bar2
  end type

  class(bar), allocatable :: sm
  type(bar2), allocatable :: sm2

  allocate (sm2)
  call move_alloc (sm,sm2) ! { dg-error "must be polymorphic if FROM is polymorphic" }

  if (allocated(sm2)) call abort()
  if (.not. allocated(sm)) call abort()
end program 

! { dg-final { cleanup-modules "m2 m3" } }
