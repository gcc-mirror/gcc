SUBROUTINE S(zin)
  COMPLEX(8), DIMENSION(3,3,3) :: zin
  INTEGER :: m,n
  CALL mltfftsg ( zin, m, n )
END SUBROUTINE

COMPLEX(8), DIMENSION(3,3,3) :: zin
CALL s(zin)
END

