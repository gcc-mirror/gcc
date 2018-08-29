! { dg-do run }
subroutine foo1 (x,y)
  use iso_fortran_env
  integer, intent(out) :: x, y

  x = numeric_storage_size
  y = character_storage_size
end

subroutine foo2 (x,y)
  use iso_fortran_env, foo => numeric_storage_size
  integer, intent(in) :: x, y

  if (foo /= x .or. character_storage_size /= y) STOP 1
end

subroutine foo3 (x,y)
  use iso_fortran_env, only : numeric_storage_size, character_storage_size
  integer, intent(in) :: x, y

  if (numeric_storage_size /= x .or. character_storage_size /= y) STOP 2
end

program test
  integer :: x, y
  call foo1(x,y)
  call foo2(x,y)
  call foo3(x,y)
end
