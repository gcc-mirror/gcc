! { dg-do compile }
!
! PR 44044: [OOP] SELECT TYPE with class-valued function
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

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
  print *,"t1"
type is (t2)
  print *,"t2"
class default
  print *,"default"
end select

select type ( y => fun(-1) )
type is (t1)
  print *,"t1"
type is (t2)
  print *,"t2"
class default
  print *,"default"
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
