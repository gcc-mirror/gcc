C { dg-do compile }
C { dg-options "-Werror -fmax-errors=1" }
100   ! { dg-error "empty statement" }
      end
subroutine foo ! Not checked ...
end function ! ... but an error
C { dg-excess-errors "warnings being treated as errors" }
