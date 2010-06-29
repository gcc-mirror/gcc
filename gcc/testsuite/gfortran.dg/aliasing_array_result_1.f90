! { dg-do run }
! Tests the fic for PR44582, where gfortran was found to
! produce an incorrect result when the result of a function
! was aliased by a host or use associated variable, to which
! the function is assigned. In these cases a temporary is
! required in the function assignments. The check has to be
! rather restrictive.  Whilst the cases marked below might
! not need temporaries, the TODOs are going to be tough.
!
! Reported by Yin Ma <yin@absoft.com> and
! elaborated by Tobias Burnus <burnus@gcc.gnu.org>
!
module foo
  INTEGER, PARAMETER :: ONE = 1
  INTEGER, PARAMETER :: TEN = 10
  INTEGER, PARAMETER :: FIVE = TEN/2
  INTEGER, PARAMETER :: TWO = 2
  integer :: foo_a(ONE)
  integer :: check(ONE) = TEN
  LOGICAL :: abort_flag = .false. 
contains
  function foo_f()
     integer :: foo_f(ONE)
     foo_f = -FIVE
     foo_f = foo_a - foo_f
  end function foo_f
  subroutine bar
    foo_a = FIVE
! This aliases 'foo_a' by host association.
    foo_a = foo_f ()
    if (any (foo_a .ne. check)) call myabort (0)
  end subroutine bar
  subroutine myabort(fl)
    integer :: fl
    print *, fl
    abort_flag = .true.
  end subroutine myabort
end module foo

function h_ext()
  use foo
  integer :: h_ext(ONE)
  h_ext = -FIVE
  h_ext = FIVE - h_ext
end function h_ext

function i_ext() result (h)
  use foo
  integer :: h(ONE)
  h = -FIVE
  h = FIVE - h
end function i_ext

subroutine tobias
  use foo
  integer :: a(ONE)
  a = FIVE
  call sub1(a)
  if (any (a .ne. check)) call myabort (1)
contains
  subroutine sub1(x)
    integer :: x(ONE)
! 'x' is aliased by host association in 'f'.
    x = f()
  end subroutine sub1
  function f()
    integer :: f(ONE)
    f = ONE
    f = a + FIVE
  end function f
end subroutine tobias

program test
  use foo
  implicit none
  common /foo_bar/ c
  integer :: a(ONE), b(ONE), c(ONE), d(ONE)
  interface
    function h_ext()
      use foo
      integer :: h_ext(ONE)
    end function h_ext
  end interface
  interface
    function i_ext() result (h)
      use foo
      integer :: h(ONE)
    end function i_ext
  end interface

  a = FIVE
! This aliases 'a' by host association
  a = f()
  if (any (a .ne. check)) call myabort (2)
  a = FIVE
  if (any (f() .ne. check)) call myabort (3)
  call bar
  foo_a = FIVE
! This aliases 'foo_a' by host association.
  foo_a = g ()
  if (any (foo_a .ne. check)) call myabort (4)
  a = FIVE
  a = h()           ! TODO: Needs no temporary
  if (any (a .ne. check)) call myabort (5)
  a = FIVE
  a = i()           ! TODO: Needs no temporary
  if (any (a .ne. check)) call myabort (6)
  a = FIVE
  a = h_ext()       ! Needs no temporary - was OK
  if (any (a .ne. check)) call myabort (15)
  a = FIVE
  a = i_ext()       ! Needs no temporary - was OK
  if (any (a .ne. check)) call myabort (16)
  c = FIVE
! This aliases 'c' through the common block.
  c = j()
  if (any (c .ne. check)) call myabort (7)
  call aaa
  call tobias
  if (abort_flag) call abort
contains
  function f()
     integer :: f(ONE)
     f = -FIVE
     f = a - f
  end function f
  function g()
     integer :: g(ONE)
     g = -FIVE
     g = foo_a - g
  end function g
  function h()
     integer :: h(ONE)
     h = -FIVE
     h = FIVE - h
  end function h
  function i() result (h)
     integer :: h(ONE)
     h = -FIVE
     h = FIVE - h
  end function i
  function j()
     common /foo_bar/ cc
     integer :: j(ONE), cc(ONE)
     j = -FIVE
     j = cc - j
  end function j
  subroutine aaa()
    d = TEN - TWO
! This aliases 'd' through 'get_d'.
    d = bbb()
    if (any (d .ne. check)) call myabort (8)
  end subroutine aaa
  function bbb()
    integer :: bbb(ONE)
    bbb = TWO
    bbb = bbb + get_d()
  end function bbb
  function get_d()
    integer :: get_d(ONE)
    get_d = d
  end function get_d
end program test
! { dg-final { cleanup-modules "foo" } }
