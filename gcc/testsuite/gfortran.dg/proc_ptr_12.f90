! { dg-do run }
!
! PR 36704: Procedure pointer as function result
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

procedure(integer),pointer :: p
p => foo()
if (p(-1)/=1) STOP 1
contains
  function foo() result(bar)
    procedure(integer),pointer :: bar
    bar => iabs
  end function
end
