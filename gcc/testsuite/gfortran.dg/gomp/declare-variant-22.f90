! PR118839:  Check that error is diagnosed when the variant is the same
! as the base function.

subroutine f()
  !$omp declare variant(f) match(user={condition(.true.)})  ! { dg-error "variant 'f' at .1. is the same as base function" }
end subroutine
