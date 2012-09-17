! { dg-do run }
!
! PR 54285: [F03] Calling a PPC with proc-ptr result
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: t
  procedure(a), pointer, nopass :: p
end type

type(t) :: x
procedure(iabs), pointer :: pp

x%p => a

pp => x%p()

if (pp(-3) /= 3) call abort

contains

  function a() result (b)
    procedure(iabs), pointer :: b
    b => iabs
  end function

end
