! { dg-do run }
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
program main
  character (len=:), allocatable :: a(:)
  allocate (character(len=10) :: a(5))
  if (sizeof(a) .ne. 50) stop 1
  deallocate (a)
end program main
