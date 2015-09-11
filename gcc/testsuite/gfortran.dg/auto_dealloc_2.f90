! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 47637: [OOP] Memory leak involving INTENT(OUT) CLASS argument w/ allocatable components
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

program test

type :: t
  integer, allocatable :: i(:)
end type

block ! New block as the main program implies SAVE
type(t) :: a

call init(a)
call init(a)
end block
contains

  subroutine init(x)
    class(t), intent(out) :: x
    allocate(x%i(1000))
  end subroutine

end program 

! { dg-final { scan-tree-dump-times "__builtin_free" 4 "original" } }
! { dg-final { scan-tree-dump-times "x->_vptr->_final \\(" 1 "original" } }
