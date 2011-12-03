! { dg-do compile }
!
! PR fortran/48887
!
! "If the selector is allocatable, it shall be allocated; the
!  associate name is associated with the data object and does
!  not have the ALLOCATABLE attribute."
!
module m
  type t
  end type t
contains
  subroutine one(a)
    class(t), allocatable :: a
    class(t), allocatable :: b
    allocate (b)
    select type (b)
      type is(t)
        call move_alloc (b, a) ! { dg-error "must be ALLOCATABLE" }
    end select
  end subroutine one

  subroutine two (a)
    class(t), allocatable :: a
    type(t), allocatable :: b
    allocate (b)
    associate (c => b)
      call move_alloc (b, c) ! { dg-error "must be ALLOCATABLE" }
    end associate
  end subroutine two
end module m

type t
end type t
class(t), allocatable :: x

select type(x)
  type is(t)
    print *, allocated (x) ! { dg-error "must be ALLOCATABLE" }
end select

select type(y=>x)
  type is(t)
    print *, allocated (y)  ! { dg-error "must be ALLOCATABLE" }
end select

associate (y=>x)
  print *, allocated (y)  ! { dg-error "must be ALLOCATABLE" }
end associate
end
