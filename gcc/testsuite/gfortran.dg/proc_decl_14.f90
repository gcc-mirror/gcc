! { dg-do compile }
! PR fortran/35830
!
abstract interface
  function ptrfunc()
    integer, pointer :: ptrfunc
  end function ptrfunc
  elemental subroutine elem(a)
    integer,intent(in) :: a
  end subroutine elem
  function dims()
    integer :: dims(3)
  end function dims
end interface

procedure(ptrfunc) :: func_a
procedure(elem)    :: func_b
procedure(dims)     :: func_c

integer, pointer :: ptr
integer :: array(3)

ptr => func_a()
call func_b([1,2,3])
array = func_c()
end
