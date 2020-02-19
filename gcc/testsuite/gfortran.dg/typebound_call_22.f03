! { dg-do compile }
! { dg-options "-fdump-tree-optimized -O" }
!
! PR 50960: [OOP] vtables not marked as constant
!
! This test case checks whether the type-bound call to "x%bar"
! is optimized into a static call to "base".
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m
  type t
  contains
    procedure, nopass :: bar => base
  end type
contains
  subroutine base()
    write(*,*) 'base'
  end subroutine
end module

program test
  use m
  class(t), allocatable :: x
  allocate (t :: x)
  call x%bar ()
end program

! { dg-final { scan-tree-dump-times "base \\(\\);" 1 "optimized" } }
