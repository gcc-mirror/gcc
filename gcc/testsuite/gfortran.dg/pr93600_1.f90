! { dg-do compile }

program p
  integer, parameter :: a(0) = 0
  character(0), parameter :: b(0) = ''
  a%kind = 1  ! { dg-error "Assignment to a constant expression" }
  b%len = 'a' ! { dg-error "Assignment to a constant expression" }
end program

