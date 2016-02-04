! { dg-options "-floop-nest-optimize -O3" }

MODULE spme
  INTEGER, PARAMETER :: dp=8
  PRIVATE
  PUBLIC :: get_patch
CONTAINS
  SUBROUTINE get_patch ( part, box, green, npts, p, rhos, is_core, is_shell,&
                         unit_charge, charges, coeff, n )
    INTEGER, POINTER                 :: box
    REAL(KIND=dp), &
      DIMENSION(-(n-1):n-1, 0:n-1), &
      INTENT(IN)                             :: coeff
    INTEGER, DIMENSION(3), INTENT(IN)        :: npts
    REAL(KIND=dp), DIMENSION(:, :, :), &
      INTENT(OUT)                            :: rhos
    REAL(KIND=dp)                            :: q
    REAL(KIND=dp), DIMENSION(3)              :: delta, r
    CALL get_delta ( box, r, npts, delta, nbox )
    CALL spme_get_patch ( rhos, nbox, delta, q, coeff )
  END SUBROUTINE get_patch
  SUBROUTINE spme_get_patch ( rhos, n, delta, q, coeff )
    REAL(KIND=dp), DIMENSION(:, :, :), &
      INTENT(OUT)                            :: rhos
    REAL(KIND=dp), DIMENSION(3), INTENT(IN)  :: delta
    REAL(KIND=dp), INTENT(IN)                :: q
    REAL(KIND=dp), &
      DIMENSION(-(n-1):n-1, 0:n-1), &
      INTENT(IN)                             :: coeff
    INTEGER, PARAMETER                       :: nmax = 12
    REAL(KIND=dp), DIMENSION(3, -nmax:nmax)  :: w_assign
    REAL(KIND=dp), DIMENSION(3, 0:nmax-1)    :: deltal
    REAL(KIND=dp), DIMENSION(3, 1:nmax)      :: f_assign
    DO l = 1, n-1
       deltal ( 3, l ) = deltal ( 3, l-1 ) * delta ( 3 )
    END DO
    DO j = -(n-1), n-1, 2
       DO l = 0, n-1
          w_assign ( 1, j ) =  w_assign ( 1, j ) + &
                         coeff ( j, l ) * deltal ( 1, l )
       END DO
       f_assign (3, i ) = w_assign ( 3, j )
       DO i2 = 1, n
          DO i1 = 1, n
             rhos ( i1, i2, i3 ) = r2 * f_assign ( 1, i1 )
          END DO
       END DO
    END DO
  END SUBROUTINE spme_get_patch
  SUBROUTINE get_delta ( box, r, npts, delta, n )
    INTEGER, POINTER :: box
    REAL(KIND=dp), DIMENSION(3), INTENT(IN)  :: r
    INTEGER, DIMENSION(3), INTENT(IN)        :: npts
    REAL(KIND=dp), DIMENSION(3), INTENT(OUT) :: delta
    INTEGER, DIMENSION(3)                    :: center
    REAL(KIND=dp), DIMENSION(3)              :: ca, grid_i, s
    CALL real_to_scaled(s,r,box)
    s = s - REAL ( NINT ( s ),KIND=dp)
    IF ( MOD ( n, 2 ) == 0 ) THEN
       ca ( : ) = REAL ( center ( : ) )
    END IF
    delta ( : ) = grid_i ( : ) - ca ( : )
  END SUBROUTINE get_delta
END MODULE spme
