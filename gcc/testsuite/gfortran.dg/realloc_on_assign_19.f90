! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR 52243 - avoid check for reallocation when doing simple
! assignments with the same variable on both sides.
module  foo
contains
  elemental function ele(a)
    real, intent(in) :: a
    real :: ele
    ele = 1./(2+a)
  end function ele

  subroutine bar(a)
    real, dimension(:), allocatable :: a
    a = a * 2.0
    a = sin(a-0.3)
    a = ele(a)
  end subroutine bar
end module foo
! { dg-final { scan-tree-dump-times "alloc" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
