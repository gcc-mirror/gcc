subroutine foo

  !$omp masked
     goto 55  ! { dg-error "invalid branch to/from OpenMP structured block" }
              ! { dg-warning "Legacy Extension: Label at .1. is not in the same block as the GOTO statement" "" { target *-*-* } .-1 }
  !$omp end masked

  !$omp masked
55  continue  ! { dg-warning "Legacy Extension: Label at .1. is not in the same block as the GOTO statement" }
    return    ! { dg-error "invalid branch to/from OpenMP structured block" }
  !$omp end masked
end subroutine foo
