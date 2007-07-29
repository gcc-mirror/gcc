! { dg-do compile }
! PR 13201 we used to not give an error in those cases
subroutine foo(n) 
  integer, parameter :: a(n) = 1           ! { dg-error "cannot be automatic" "automatic shape" }
  integer, parameter :: z(:) = (/ 1,2,3 /) ! { dg-error "cannot be automatic" "deferred shape" }
end subroutine
