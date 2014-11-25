! { dg-do compile }
! { dg-options "-Werror -fmax-errors=1" }
100   ! { dg-error "empty statement" }
end
subroutine foo ! Not checked ...
end function ! ... but an error
! { dg-excess-errors "warnings being treated as errors" }
