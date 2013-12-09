! { dg-do compile }
! Test fix for PR54286.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
! Module 'm' added later because original fix missed possibility of
! null interfaces - thanks to Dominique Dhumieres  <dominiq@lps.ens.fr>
!
module m
  type :: foobar
    real, pointer :: array(:)
    procedure (), pointer, nopass :: f
  end type
contains
  elemental subroutine fooAssgn (a1, a2)
    type(foobar), intent(out) :: a1
    type(foobar), intent(in) :: a2
    allocate (a1%array(size(a2%array)))
    a1%array = a2%array
    a1%f => a2%f
  end subroutine
end module m

implicit integer (a)
type :: t
  procedure(a), pointer, nopass :: p
end type
type(t) :: x

! We cannot use iabs directly as it is elemental
abstract interface
  integer pure function interf_iabs(x)
    integer, intent(in) :: x
  end function interf_iabs
end interface

procedure(interf_iabs), pointer :: pp
procedure(foo), pointer :: pp1

x%p => a     ! ok
if (x%p(0) .ne. loc(foo)) call abort
if (x%p(1) .ne. loc(iabs)) call abort

x%p => a(1)  ! { dg-error "PROCEDURE POINTER mismatch in function result" }

pp => a(1)   ! ok
if (pp(-99) .ne. iabs(-99)) call abort

pp1 => a(2)   ! ok
if (pp1(-99) .ne. -iabs(-99)) call abort

pp => a  ! { dg-error "PROCEDURE POINTER mismatch in function result" }

contains

  function a (c) result (b)
    integer, intent(in) :: c
    procedure(interf_iabs), pointer :: b
    if (c .eq. 1) then
      b => iabs
    else
      b => foo
    end if
  end function

  pure integer function foo (arg)
    integer, intent (in) :: arg
    foo = -iabs(arg)
  end function
end
