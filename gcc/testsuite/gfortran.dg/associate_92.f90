! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! A CLASS array component referenced through an associate name whose CLASS
! type was inferred lost its polymorphism, so SAME_TYPE_AS folded to false.
!
module m
  type :: leaf_t
     integer :: bar = 0
  end type
  type, extends (leaf_t) :: leaf2_t
     integer :: baz = 7
  end type
  type :: mid_t
     class(leaf_t), allocatable :: c2(:)
  end type
  type :: outer_t
     class(mid_t), allocatable :: c1(:)
  end type
end module

program p
  use m
  implicit none

  associate (q => gfcn ())
    if (.not. same_type_as (q%c1(4)%c2, leaf2_t ())) stop 1
    if (same_type_as (q%c1(4)%c2, leaf_t ())) stop 2
    if (.not. same_type_as (q%c1(4)%c2(2:3), leaf2_t ())) stop 3
  end associate

contains

  function gfcn () result (y)
    class(outer_t), allocatable :: y
    allocate (y)
  end function
end program

! { dg-final { scan-tree-dump-times "c2\\._vptr->_hash" 3 "original" } }
