! Related to PR 15326.  Test calls to string functions whose lengths
! depend on various types of scalar value.
! { dg-do run }
pure function select (selector, iftrue, iffalse)
  logical, intent (in) :: selector
  integer, intent (in) :: iftrue, iffalse
  integer :: select

  if (selector) then
    select = iftrue
  else
    select = iffalse
  end if
end function select

program main
  implicit none

  interface
    pure function select (selector, iftrue, iffalse)
      logical, intent (in) :: selector
      integer, intent (in) :: iftrue, iffalse
      integer :: select
    end function select
  end interface

  type pair
    integer :: left, right
  end type pair

  integer, target :: i
  integer, pointer :: ip
  real, target :: r
  real, pointer :: rp
  logical, target :: l
  logical, pointer :: lp
  complex, target :: c
  complex, pointer :: cp
  character, target :: ch
  character, pointer :: chp
  type (pair), target :: p
  type (pair), pointer :: pp

  character (len = 10) :: dig

  i = 100
  r = 50.5
  l = .true.
  c = (10.9, 11.2)
  ch = '1'
  p%left = 40
  p%right = 50

  ip => i
  rp => r
  lp => l
  cp => c
  chp => ch
  pp => p

  dig = '1234567890'

  call test (f1 (i), 200)
  call test (f1 (ip), 200)
  call test (f1 (-30), 60)
  call test (f1 (i / (-4)), 50)

  call test (f2 (r), 100)
  call test (f2 (rp), 100)
  call test (f2 (70.1), 140)
  call test (f2 (r / 4), 24)
  call test (f2 (real (i)), 200)

  call test (f3 (l), 50)
  call test (f3 (lp), 50)
  call test (f3 (.false.), 55)
  call test (f3 (i < 30), 55)

  call test (f4 (c), 10)
  call test (f4 (cp), 10)
  call test (f4 (cmplx (60.0, r)), 60)
  call test (f4 (cmplx (r, 1.0)), 50)

  call test (f5 (ch), 11)
  call test (f5 (chp), 11)
  call test (f5 ('23'), 12)
  call test (f5 (dig (3:)), 13)
  call test (f5 (dig (10:)), 10)

  call test (f6 (p), 145)
  call test (f6 (pp), 145)
  call test (f6 (pair (20, 10)), 85)
  call test (f6 (pair (i / 2, 1)), 106)
contains
  function f1 (i)
    integer :: i
    character (len = abs (i) * 2) :: f1
    f1 = ''
  end function f1

  function f2 (r)
    real :: r
    character (len = floor (r) * 2) :: f2
    f2 = ''
  end function f2

  function f3 (l)
    logical :: l
    character (len = select (l, 50, 55)) :: f3
    f3 = ''
  end function f3

  function f4 (c)
    complex :: c
    character (len = int (c)) :: f4
    f4 = ''
  end function f4

  function f5 (c)
    character :: c
    character (len = scan ('123456789', c) + 10) :: f5
    f5 = ''
  end function f5

  function f6 (p)
    type (pair) :: p
    integer :: i
    character (len = sum ((/ p%left, p%right, (i, i = 1, 10) /))) :: f6
    f6 = ''
  end function f6

  subroutine test (string, length)
    character (len = *) :: string
    integer, intent (in) :: length
    if (len (string) .ne. length) call abort
  end subroutine test
end program main
