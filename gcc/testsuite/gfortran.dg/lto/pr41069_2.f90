SUBROUTINE fftsg3d ( n, zout )
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND ( 14, 200 )
  INTEGER, DIMENSION(*), INTENT(IN)                    :: n
  COMPLEX(KIND=dp), DIMENSION(*), INTENT(INOUT)        :: zout
  INTEGER                                              :: nx
  nx = n ( 1 )
  CALL mltfftsg ( zout, nx, nx )
END SUBROUTINE fftsg3d

