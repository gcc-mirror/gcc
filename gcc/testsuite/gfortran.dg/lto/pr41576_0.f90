! { dg-lto-do run }
! { dg-lto-options { { -O2 -flto -Werror } } }

subroutine foo
  common /bar/ a, b
  integer(4) :: a ,b
  a = 1
  b = 2
end

