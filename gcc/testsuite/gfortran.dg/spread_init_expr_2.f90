! { dg-do compile }
! { dg-options "-std=f95" }
module bug
  integer :: ibug(42) = spread(42, 1, 42) ! { dg-error "invalid in an initialization expression" }
end module
