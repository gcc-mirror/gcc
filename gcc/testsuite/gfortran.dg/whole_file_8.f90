! { dg-do compile }
! { dg-options "-fwhole-file" }
! Test the fix for the third problem in PR40011, where false
! type/rank mismatches were found in the main program calls.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!
subroutine test_d(fn, val, res)
  double precision fn
  double precision val, res

  print *, fn(val), res
end subroutine

subroutine test_c(fn, val, res)
  complex fn
  complex val, res

  print *, fn(val), res
end subroutine

program specifics

  intrinsic dcos
  intrinsic dcosh
  intrinsic dexp

  intrinsic conjg

  call test_d (dcos, 1d0, dcos(1d0))
  call test_d (dcosh, 1d0, dcosh(1d0))
  call test_d (dexp, 1d0, dexp(1d0))

  call test_c (conjg, (1.0,1.0) , conjg((1.0,1.0)))

end program
