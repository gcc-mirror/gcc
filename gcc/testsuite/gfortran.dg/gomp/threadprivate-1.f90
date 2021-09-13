! PR fortran/99514
!
! NTest in DATA is implicitly SAVE, unless in COMMON
! Was failing before as the implicit SAVE was not
! honoured by the threadprivate check.
!

program main
  DATA NTest /1/
  !$omp threadprivate(Ntest)
end program main
