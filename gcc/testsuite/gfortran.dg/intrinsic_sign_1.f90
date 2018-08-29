! { dg-do run }
! At one point, SIGN() evaluated its first argument twice.
! Contributed by Brooks Moses <brooks.moses@codesourcery.com>
program sign1
  integer :: i
  i = 1
  if (sign(foo(i), 1) /= 1) STOP 1
  i = 1
  if (sign(foo(i), -1) /= -1) STOP 2
contains
  integer function foo(i)
    integer :: i
    foo = i
    i = i + 1
  end function
end
