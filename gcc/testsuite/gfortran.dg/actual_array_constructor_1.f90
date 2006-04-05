! { dg-do run }
! Test the fix by HJ Lu for PR23634 and friends. All involve the ICE
! that arose from a character array constructor usedas an actual
! argument.
!
! The various parts of this test are taken from the PRs.
!
! Test PR26491
module global
  public    p, line
  interface p
    module procedure p
  end interface
  character(128) :: line = 'abcdefghijklmnopqrstuvwxyz'
contains
  subroutine p()
    character(128) :: word
    word = line
    call redirect_((/word/))
  end subroutine
  subroutine redirect_ (ch)
    character(*) :: ch(:)
    if (ch(1) /= line) call abort ()
  end subroutine redirect_
end module global

! Test PR26550
module my_module
  implicit none
  type point
    real :: x
  end type point
  type(point), pointer, public :: stdin => NULL()
contains
  subroutine my_p(w)
    character(128) :: w
    call r(stdin,(/w/))
  end subroutine my_p
  subroutine r(ptr, io)
    use global
    type(point), pointer :: ptr
    character(128) :: io(:)
    if (associated (ptr)) call abort ()
    if (io(1) .ne. line) call abort ()
  end subroutine r
end module my_module

program main
  use global
  use my_module

  integer :: i(6) = (/1,6,3,4,5,2/)
  character (6) :: a = 'hello ', t
  character(len=1) :: s(6) = (/'g','g','d','d','a','o'/)
  equivalence (s, t)

  call option_stopwatch_s (a) ! Call test of PR25619
  call p ()                   ! Call test of PR26491
  call my_p (line)            ! Call test of PR26550

! Test Vivek Rao's bug, as reported in PR25619.
  s = s(i)
  call option_stopwatch_a ((/a,'hola! ', t/))

contains

! Test PR23634
  subroutine option_stopwatch_s(a)
    character (*), intent(in) :: a
    character (len=len(a)) :: b

    b = 'hola! '
    call option_stopwatch_a((/a, b, 'goddag'/))
  end subroutine option_stopwatch_s 
  subroutine option_stopwatch_a (a)
    character (*) :: a(:)
    if (any (a .ne. (/'hello ','hola! ','goddag'/))) call abort ()
  end subroutine option_stopwatch_a

end program main
! { dg-final { cleanup-modules "global my_module" } }

