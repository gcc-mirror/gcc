! { dg-do compile }
! { dg-options "-O3" }
SUBROUTINE calculates_green_opt()
  INTEGER, PARAMETER :: dp=8
  REAL(KIND=dp), ALLOCATABLE, DIMENSION(:) :: green, green1
  ALLOCATE(green(-nrec:nrec),stat=i_all)
  DO ikern=1,nrec
     green(-ikern)=gleft+gright
     IF (ABS(green(ikern)) <= 1.e-20_dp) THEN
        nrec=ikern
        EXIT
     END IF
  END DO
  ALLOCATE(green1(-nrec:nrec),stat=i_all)
  CALL scf_recursion(nrec,green(-nrec),green1(-nrec))
END SUBROUTINE calculates_green_opt

