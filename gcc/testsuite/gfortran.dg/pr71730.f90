! { dg-do compile }
subroutine foo
  implicit none
  character(len=bar) :: a ! { dg-error "Scalar INTEGER expression" }
end subroutine foo
