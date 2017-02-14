! PR fortran/72716
! { dg-do compile }

block data
  !$omp declare simd (z)	! { dg-error "statement is not allowed inside of BLOCK DATA" }
end block data
