! { dg-do run }
!
! PR 54285: [F03] Calling a PPC with proc-ptr result
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: t
  procedure(a), pointer, nopass :: p
end type

type(t) :: x

! We cannot use "iabs" directly as it is elemental.
abstract interface
  pure integer function interf_iabs(x)
    integer, intent(in) :: x
  end function interf_iabs
end interface
procedure(interf_iabs), pointer :: pp

x%p => a

pp => x%p()

if (pp(-3) /= 3) STOP 1

contains

  function a() result (b)
    procedure(interf_iabs), pointer :: b
    b => iabs
  end function

end
