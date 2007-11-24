! { dg-do run }
! { dg-options "-O1" }
! Checks the fix for PR33541, in which a requirement of
! F95 11.3.2 was not being met: The local names 'x' and
! 'y' coming from the USE statements without an ONLY clause
! should not survive in the presence of the locally renamed
! versions. In fixing the PR, the same correction has been
! made to generic interfaces.
!
! Reported by Reported by John Harper in
! http://gcc.gnu.org/ml/fortran/2007-09/msg00397.html
!
MODULE xmod
  integer(4) :: x = -666
  private foo, bar
  interface xfoobar
    module procedure foo, bar
  end interface
contains
  integer function foo ()
    foo = 42
  end function
  integer function bar (a)
    integer a
    bar = a
  end function
END MODULE xmod

MODULE ymod
  integer(4) :: y = -666
  private foo, bar
  interface yfoobar
    module procedure foo, bar
  end interface
contains
  integer function foo ()
    foo = 42
  end function
  integer function bar (a)
    integer a
    bar = a
  end function
END MODULE ymod

  integer function xfoobar () ! These function as defaults should...
    xfoobar = 99
  end function

  integer function yfoobar () ! ...the rename works correctly.
    yfoobar = 99
  end function

PROGRAM test2uses
  implicit integer(2) (a-z)
  x = 666  ! These assignments generate implicitly typed
  y = 666  ! local variables 'x' and 'y'.
  call test1
  call test2
  call test3
contains
  subroutine test1  ! Test the fix of the original PR
    USE xmod
    USE xmod, ONLY: xrenamed => x
    USE ymod, ONLY: yrenamed => y
    USE ymod
    implicit integer(2) (a-z)
    if (kind(xrenamed) == kind(x)) call abort ()
    if (kind(yrenamed) == kind(y)) call abort ()
  end subroutine

  subroutine test2  ! Test the fix applies to generic interfaces
    USE xmod
    USE xmod, ONLY: xfoobar_renamed => xfoobar
    USE ymod, ONLY: yfoobar_renamed => yfoobar
    USE ymod
    if (xfoobar_renamed (42) == xfoobar ()) call abort ()
    if (yfoobar_renamed (42) == yfoobar ()) call abort ()
  end subroutine

  subroutine test3  ! Check that USE_NAME == LOCAL_NAME is OK
    USE xmod
    USE xmod, ONLY: x => x, xfoobar => xfoobar
    USE ymod, ONLY: y => y, yfoobar => yfoobar
    USE ymod
    if (kind (x) /= 4) call abort ()    
    if (kind (y) /= 4) call abort ()    
    if (xfoobar (77) /= 77_4) call abort ()
    if (yfoobar (77) /= 77_4) call abort ()
  end subroutine
END PROGRAM test2uses
! { dg-final { cleanup-modules "xmod ymod" } }
