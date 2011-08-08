! { dg-do run }
! PR 25031 - We didn't cause an error when allocating an already
!            allocated array.
!
! This testcase has been modified to fix PR 49755. 
program alloc_test
  implicit none
  integer :: i
  integer, allocatable :: a(:)
  integer, pointer :: b(:)

  allocate(a(4))
  ! This should set the stat code but not change the size.
  allocate(a(3),stat=i)
  if (i == 0) call abort
  if (.not. allocated(a)) call abort
  if (size(a) /= 4) call abort

  ! It's OK to allocate pointers twice (even though this causes
  ! a memory leak)
  allocate(b(4))
  allocate(b(4))
end program
