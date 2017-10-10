! { dg-do compile }
! { dg-options "-O2 -floop-nest-optimize" }

SUBROUTINE print_crys_symmetry(nc,v)
  INTEGER :: nc
  REAL(KIND=8), DIMENSION(3,48) :: v
  INTEGER  :: n,i
  vs = 0.0_8
  DO n = 1, nc 
     DO i = 1, 3
        vs = vs + ABS(v(i,n))
     END DO
  END DO
  CALL foo(vs)
END SUBROUTINE print_crys_symmetry
