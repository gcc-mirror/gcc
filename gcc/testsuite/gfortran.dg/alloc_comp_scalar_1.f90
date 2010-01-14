! { dg-do run }
! Test the fix for comment #8 of PR41478, in which copying
! allocatable scalar components caused a segfault.
! 
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
program main
  type :: container_t
    integer, allocatable :: entry
  end type container_t
  type(container_t), dimension(1) :: a1, a2
  allocate (a1(1)%entry, a2(1)%entry)
  a2(1)%entry = 1
  a1(1:1) = pack (a2(1:1), mask = [.true.])
  deallocate (a2(1)%entry)
  if (a1(1)%entry .ne. 1) call abort
end program main
