! { dg-do run }
!
! Test of fix for case in comment #7 of PR84088.
!
! Contributed by Tom de Vries  <vries@gcc.gnu.org>
!
implicit none
  integer(kind=4) z

  call foo (z)

contains
  subroutine foo (a)
    type (*), dimension (..), contiguous :: a
    integer(kind = 4) :: i
    if(sizeof (a) .ne. sizeof (i)) STOP 1
  end subroutine foo

end program
