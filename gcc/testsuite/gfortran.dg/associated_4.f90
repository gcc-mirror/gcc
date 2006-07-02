! { dg-do run }
! PR fortran/26801
  implicit none

  integer :: i
  integer,target :: u
  logical :: l
  character(len=8) :: A
  type dt
    integer, pointer :: a => NULL()
  end type dt
  type(dt) :: obj(2)

  i = 2
  l = associated(obj(i)%a)
  write(A,*) l
  l = associated(obj(i)%a,u)
  print *, l
  write(A,*) l
end
