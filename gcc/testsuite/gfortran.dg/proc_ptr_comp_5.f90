! { dg-do run }
!
! PR39630: Fortran 2003: Procedure pointer components.
!
! Nested types / double component references.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

abstract interface
  subroutine as
  end subroutine
  integer function af()
  end function
end interface

type :: t1
  procedure(as), pointer, nopass :: s
  procedure(af), pointer, nopass :: f
end type

type :: t2
  type(t1) :: c
end type

type(t2) :: x
integer :: j = 0

x%c%s => is
call x%c%s
if (j/=5) call abort

x%c%f => if
j=x%c%f()
if (j/=42) call abort

contains

subroutine is
  j = 5
end subroutine

integer function if()
  if = 42
end function

end

