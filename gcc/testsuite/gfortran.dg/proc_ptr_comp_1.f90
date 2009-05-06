! { dg-do run }
!
! PR39630: Fortran 2003: Procedure pointer components.
!
! Basic test for PPCs with SUBROUTINE interface and NOPASS.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  type t
    integer :: i
    procedure(sub), pointer, nopass :: ppc
    procedure(), pointer, nopass :: proc
  end type

  type, extends(t) :: t2
    procedure(), pointer, nopass :: proc2
  end type t2

  type(t) :: x
  type(t2) :: x2

  procedure(sub),pointer :: pp
  integer :: sum = 0

  x%i = 1
  x%ppc => sub
  pp => x%ppc

  call sub(1)
  if (sum/=1) call abort
  call pp(2)
  if (sum/=3) call abort
  call x%ppc(3)
  if (sum/=6) call abort

  ! calling object as argument
  x%proc => sub2
  call x%proc(x)
  if (x%i/=7) call abort

  ! type extension
  x%proc => sub
  call x%proc(4)
  if (sum/=10) call abort
  x2%proc => sub
  call x2%proc(5)
  if (sum/=15) call abort
  x2%proc2 => sub
  call x2%proc2(6)
  if (sum/=21) call abort

contains

  subroutine sub(y)
    integer, intent(in) :: y
    sum = sum + y
  end subroutine

  subroutine sub2(arg)
    type(t),intent(inout) :: arg
    arg%i = arg%i + sum
  end subroutine

end

