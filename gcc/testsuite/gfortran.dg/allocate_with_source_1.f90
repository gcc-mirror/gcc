! { dg-do run }
! Test the fix for PR47592, in which the SOURCE expression was
! being called twice.
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
module foo
  implicit none
contains
  function bar()
    integer bar
    integer :: i=9
    i = i + 1
    bar = i
  end function bar
end module foo

program note7_35
  use foo
  implicit none
  character(:), allocatable :: name
  character(:), allocatable :: src
  integer n
  n = 10
  allocate(name, SOURCE=repeat('x',bar()))
  if (name .ne. 'xxxxxxxxxx') call abort
  if (len (name) .ne. 10 ) call abort
end program note7_35
