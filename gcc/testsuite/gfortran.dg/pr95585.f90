! { dg-do compile }

program test
  integer, parameter :: a(2) = reshape([1, 2], a) ! { dg-error "before its definition" }
end program

