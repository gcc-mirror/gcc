! { dg-additional-options "-Wno-openmp" }
PROGRAM master_construct_dep
  INTEGER X
  INTEGER Y
  X = 0
  Y = 8
!$OMP PARALLEL
    DO WHILE( Y > 0 )
!$OMP SINGLE
      Y = Y - 1
!$OMP END SINGLE
!$OMP MASTER ! { dg-warning "'master' construct at \\(1\\) deprecated since OpenMP 5.1, use 'masked' \\\[-Wdeprecated-openmp\\\]" }
      X = X + 1
!$OMP END MASTER
!$OMP BARRIER
    ENDDO
!$OMP END PARALLEL
END PROGRAM
