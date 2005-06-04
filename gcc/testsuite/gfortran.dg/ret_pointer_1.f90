! { dg-do run }
! Test functions returning array pointers
program ret_pointer_1
  integer, pointer, dimension(:) :: a
  integer, target, dimension(2) :: b
  integer, pointer, dimension (:) :: p

  a => NULL()
  a => foo()
  p => b
  if (.not. associated (a, p)) call abort
contains
subroutine bar(p)
  integer, pointer, dimension(:) :: p
end subroutine
function foo() result(r)
  integer, pointer, dimension(:) :: r

  r => b
end function
end program

