! { dg-do run }
!
! Test the fix for PR84074
!
! Contributed by Vladimir Fuka  <vladimir.fuka@gmail.com>
!
  type :: t
      integer :: n
  end type

  type(t) :: array(4) = [t(1),t(2),t(3),t(4)]

  call sub(array((/3,1/)), [3,1,0,0]) ! Does not increment any elements of 'array'.
  call sub(array(1:3:2), [1,3,0,0])
  call sub(array(3:1:-2), [4,2,0,0])
  call sub(array, [3,2,5,4])          ! Elements 1 and 3 should have been incremented twice.

contains

  subroutine sub(a, iarray)
    class(t) :: a(:)
    integer :: iarray(4)
    integer :: i
    do i=1,size(a)
        if (a(i)%n .ne. iarray(i)) STOP 1
        a(i)%n = a(i)%n+1
    enddo
  end subroutine
end program
