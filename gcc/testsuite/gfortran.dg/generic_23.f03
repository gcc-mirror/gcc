! { dg-do run }
! Test the fix for PR43945 in which the over-ridding of 'doit' and
! 'getit' in type 'foo2' was missed in the specific binding to 'do' and 'get'.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
! and reported to clf by Salvatore Filippone <sfilippone@uniroma2.it>
!
module foo_mod
  type foo
    integer :: i
  contains
    procedure, pass(a) :: doit
    procedure, pass(a) :: getit
    generic, public :: do  => doit
    generic, public :: get => getit
  end type foo
  private doit,getit
contains
  subroutine  doit(a)
    class(foo) :: a
    a%i = 1
    write(*,*) 'FOO%DOIT base version'
  end subroutine doit
  function getit(a) result(res)
    class(foo) :: a
    integer :: res
    res = a%i
  end function getit
end module foo_mod

module foo2_mod
  use foo_mod
  type, extends(foo) :: foo2
    integer :: j
  contains
    procedure, pass(a) :: doit  => doit2
    procedure, pass(a) :: getit => getit2
!!$    generic, public :: do  => doit
!!$    generic, public :: get => getit
  end type foo2
  private doit2, getit2

contains

  subroutine  doit2(a)
    class(foo2) :: a
    a%i = 2
    a%j = 3
  end subroutine doit2
  function getit2(a) result(res)
    class(foo2) :: a
    integer :: res
    res = a%j
  end function getit2
end module foo2_mod

program testd15
  use foo2_mod
  type(foo2) :: af2

  call af2%do()
  if (af2%i .ne. 2) call abort
  if (af2%get() .ne. 3) call abort

end program testd15

! { dg-final { cleanup-modules "foo_mod foo2_mod" } }
