! PR rtl-optimization/55330
! { dg-do compile }
! { dg-options "-O -fPIC -fno-dse -fno-guess-branch-probability" }

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

  call option_stopwatch_s (a)
  call p ()
  call my_p (line)

  s = s(i)
  call option_stopwatch_a ((/a,'hola! ', t/))

contains

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
