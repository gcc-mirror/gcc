! { dg-do run }
!
! Test the fix for PR64120 in which the initialisation of the
! string length of 's' was not being done.
!
! Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
!
   call g(1)
   call g(2)
contains
  subroutine g(x)
      integer :: x
      character(len=x), allocatable :: s
      allocate(s)
      if (len(s) .ne. x) stop x
  end subroutine
end
