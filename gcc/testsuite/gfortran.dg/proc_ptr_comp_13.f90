! { dg-do run }
!
! PR 40882: [F03] infinite recursion in gfc_get_derived_type with PPC returning derived type.
! At the same time, check that a formal argument does not cause infinite recursion (PR 40870).
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none

type :: t
  integer :: data
  procedure(foo), pointer, nopass :: ppc
  procedure(type(t)), pointer, nopass :: ppc2
end type

type(t) :: o,o2

o%data = 1
o%ppc => foo

o2 = o%ppc(o)

if (o%data /= 1) STOP 1
if (o2%data /= 5) STOP 2
if (.not. associated(o%ppc)) STOP 3
if (associated(o2%ppc)) STOP 4

contains

  function foo(arg)
    type(t) :: foo, arg
    foo%data = arg%data * 5
    foo%ppc => NULL()
  end function

end

