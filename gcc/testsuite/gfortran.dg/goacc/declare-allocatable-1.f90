! Verify that OpenACC declared allocatable arrays have implicit
! OpenACC enter and exit pragmas at the time of allocation and
! deallocation.

! { dg-additional-options "-fdump-tree-original" }

program allocate
  implicit none
  integer, allocatable :: a(:), b
  integer, parameter :: n = 100
  integer i
  !$acc declare create(a,b)

  allocate (a(n), b)

  !$acc parallel loop copyout(a, b)
  do i = 1, n
     a(i) = b
  end do

  deallocate (a, b)
end program allocate

! { dg-final { scan-tree-dump-times "pragma acc enter data map.declare_allocate" 2 "original" } }
! { dg-final { scan-tree-dump-times "pragma acc exit data map.declare_deallocate" 2 "original" } }
