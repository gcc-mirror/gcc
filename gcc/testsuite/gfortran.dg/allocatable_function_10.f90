! { dg-do run }
!
! Test the fix for PR78293. The deallocations are present at the
! end of the main programme to aid memory leak searching. The
! allocation in 'tt' leaked memory from an intermediate temporary
! for the array constructor.
!
! Contributed by Andrew Benson  <abensonca@gmail.com>
!
module m
  implicit none

  type t
     integer, allocatable, dimension(:) :: r
  end type t

contains

  function tt(a,b)
    implicit none
    type(t), allocatable, dimension(:) :: tt
    type(t), intent(in), dimension(:) :: a,b
    allocate(tt, source = [a,b])
  end function tt

  function ts(arg)
    implicit none
    type(t), allocatable, dimension(:) :: ts
    integer, intent(in) :: arg(:)
    allocate(ts(1))
    allocate(ts(1)%r, source = arg)
    return
  end function ts

end module m

program p
  use m
  implicit none
  type(t), dimension(2) :: c
  c=tt(ts([99,199,1999]),ts([42,142]))
  if (any (c(1)%r .ne. [99,199,1999])) STOP 1
  if (any (c(2)%r .ne. [42,142])) STOP 2
  deallocate(c(1)%r)
  deallocate(c(2)%r)
end program p
