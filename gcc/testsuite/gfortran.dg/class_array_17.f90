! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/57456
!
module m
  implicit none
  type t
    integer :: i
   end type t
  type, extends(t) :: t2
    integer :: j
   end type t2
end module m

program test
  use m
  implicit none
  integer :: i
  class(t), save, allocatable :: y(:)

  allocate (t2 :: y(5))
  select type(y)
  type is (t2)
    do i = 1, 5
      y(i)%i = i
      y(i)%j = i*10
    end do
  end select
  deallocate(y)
end

! { dg-final { scan-tree-dump-times "__builtin_malloc \\(40\\);" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
