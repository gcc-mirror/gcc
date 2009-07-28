! { dg-do run }
!
! PR 40882: [F03] infinite recursion in gfc_get_derived_type with PPC returning derived type
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none

type :: t
  integer :: data
  procedure(foo), pointer, nopass :: ppc
end type

type(t) :: o,o2

o%data = 1
o%ppc => foo

o2 = o%ppc()

if (o%data /= 1) call abort()
if (o2%data /= 5) call abort()
if (.not. associated(o%ppc)) call abort()
if (associated(o2%ppc)) call abort()

contains

  function foo()
    type(t) :: foo
    foo%data = 5
    foo%ppc => NULL()
  end function

end

