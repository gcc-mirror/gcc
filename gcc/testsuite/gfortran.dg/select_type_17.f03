! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/44044
! Definability check for select type to expression.
! This is "bonus feature #2" from comment #3 of the PR.

! Contributed by Janus Weil, janus@gcc.gnu.org.

implicit none

type :: t1
  integer :: i
end type

type, extends(t1) :: t2
end type

type(t1),target :: x1
type(t2),target :: x2

select type ( y => fun(1) )
type is (t1)
  y%i = 1 ! { dg-error "variable definition context" }
type is (t2)
  y%i = 2 ! { dg-error "variable definition context" }
end select

contains

  function fun(i)
    class(t1),pointer :: fun
    integer :: i
    if (i>0) then
      fun => x1
    else if (i<0) then
      fun => x2
    else
      fun => NULL()
    end if
  end function

end

