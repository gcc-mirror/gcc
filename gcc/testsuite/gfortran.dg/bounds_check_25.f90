! { dg-do run }
! { dg-additional-options "-fcheck=bounds -fdump-tree-original" }
!
! PR fortran/86100 - bogus bounds check with assignment, class component

program p
  implicit none
  type any_matrix
     class(*), allocatable :: m(:,:)
  end type any_matrix
  type(any_matrix) :: a, b
  allocate (a%m, source=reshape([3,5],shape=[1,2]))

  ! The following assignment did create a bogus bounds violation:
  b = a ! Line 15
  if (any (shape (b%m) /= shape (a%m))) stop 1

contains

  ! Verify improved array name in array name
  subroutine bla ()
    type(any_matrix) :: c, d
    allocate (real :: c%m(3,5))
    allocate (d%m(7,9),source=c%m) ! Line 24
  end subroutine bla
end

! { dg-final { scan-tree-dump-times "line 15 .* bound mismatch for dimension 1 of array .'.*.'" 1 "original" } }
! { dg-final { scan-tree-dump-times "line 15 .* bound mismatch for dimension 2 of array .'.*.'" 1 "original" } }

! { dg-final { scan-tree-dump-times "line 24 .* bound mismatch for dimension 1 of array .'d%%m.'" 1 "original" } }
! { dg-final { scan-tree-dump-times "line 24 .* bound mismatch for dimension 2 of array .'d%%m.'" 1 "original" } }
