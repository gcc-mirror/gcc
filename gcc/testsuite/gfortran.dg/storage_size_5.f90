! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
subroutine test()
  implicit none
  integer :: i0, i1, i2, i3, i4
  i0 = kind(STORAGE_SIZE(5))
  i1 = kind(STORAGE_SIZE(5, kind=1))
  i2 = kind(STORAGE_SIZE(5, kind=2))
  i3 = kind(STORAGE_SIZE(5, kind=4))
  i4 = kind(STORAGE_SIZE(5, kind=8))
end subroutine test

subroutine test2(x)
  implicit none
  class(*) :: x
  integer :: j0, j1, j2, j3, j4
  integer(1) :: k1
  integer(2) :: k2
  j0 = kind(STORAGE_SIZE(x))
  j1 = kind(STORAGE_SIZE(x, kind=1))
  j2 = kind(STORAGE_SIZE(x, kind=2))
  j3 = kind(STORAGE_SIZE(x, kind=4))
  j4 = kind(STORAGE_SIZE(x, kind=8))

  k1 = STORAGE_SIZE(x, kind=1)
  k2 = STORAGE_SIZE(x, kind=2)
end subroutine test2

! { dg-final { scan-tree-dump-times "i0 = 4;" 1 "original" } }
! { dg-final { scan-tree-dump-times "i1 = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "i2 = 2;" 1 "original" } }
! { dg-final { scan-tree-dump-times "i3 = 4;" 1 "original" } }
! { dg-final { scan-tree-dump-times "i4 = 8;" 1 "original" } }
! { dg-final { scan-tree-dump-times "j0 = 4;" 1 "original" } }

! { dg-final { scan-tree-dump-times "j1 = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "j2 = 2;" 1 "original" } }
! { dg-final { scan-tree-dump-times "j3 = 4;" 1 "original" } }
! { dg-final { scan-tree-dump-times "j4 = 8;" 1 "original" } }

! { dg-final { scan-tree-dump-times "k1 = \\(integer\\(kind=1\\)\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "k2 = \\(integer\\(kind=2\\)\\)" 1 "original" } }
