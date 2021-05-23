! { dg-do run }
! PR fortran/100551 - Passing return value to class(*) dummy argument

program p
  implicit none
  integer :: result
  result = 1
  result = test (    (result)) ! works
  if (result /= 1) stop 1
  result = test (int (result)) ! issue 1
! write(*,*) result
  if (result /= 1) stop 2
  result = test (f   (result)) ! issue 2
! write(*,*) result
  if (result /= 2) stop 3
contains
  integer function test(x)
    class(*), intent(in) :: x
    select type (x)
    type is (integer)
       test = x
    class default
       test = -1
    end select
  end function test
  integer function f(x)
    integer, intent(in) :: x
    f = 2*x
  end function f
end program
