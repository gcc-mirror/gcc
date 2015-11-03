! { dg-do compile }
! { dg-options "-Werror" }
subroutine s
  type t
  1 ! { dg-error "empty statement" }
  end type
end subroutine
! { dg-excess-errors "warnings being treated as errors" }
