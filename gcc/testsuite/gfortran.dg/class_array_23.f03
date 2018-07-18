! { dg-do run }
!
! Test the fix for PR84538 in which the scalarizer was taking the size
! of 't', rather than 'te', to generate array references.
!
! Contributed by Andrew Benson  <abensonca@gmail.com>
!
module bugMod
  public
  type :: t
     integer :: i
  end type t
  type, extends(t) :: te
     integer :: j
  end type te
contains
  subroutine check(n)
    implicit none
    class(t), intent(inout), dimension(:) :: n
    integer :: i(2)
    i = n%i ! Original testcase had this in a write statement. However,
            ! it is the scalarizer that is getting the span wrong and so
            ! this assignment failed too.
    if (any (i .ne. [8,3])) stop 1
    return
  end subroutine check
end module bugMod

program bug
  use bugMod
  class(t), allocatable, dimension(:) :: n
  allocate(te :: n(2))
  n(1:2)%i=[8,3]
  if (any (n%i .ne. [8,3])) stop 2
  call check(n)
  deallocate (n)
end program bug
