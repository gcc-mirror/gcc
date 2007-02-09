! { dg-do run }
! PR fortran/30720
program array_function_1
  integer :: a(5), b, l, u
  l = 4
  u = 2

  a = (/ 1, 2, 3, 4, 5 /)

  b = f(a(l:u) - 2)
  if (b /= 0) call abort

  b = f(a(4:2) - 2)
  if (b /= 0) call abort

  b = f(a(u:l) - 2)
  if (b /= 3) call abort

  b = f(a(2:4) - 2)
  if (b /= 3) call abort

  contains
    integer function f(x)
      integer, dimension(:), intent(in) :: x
      f = sum(x)
    end function
end program
