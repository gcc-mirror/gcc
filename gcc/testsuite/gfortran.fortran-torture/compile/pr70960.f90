  SUBROUTINE calbrec(a,ai,error)
    REAL(KIND=8)                            :: a(3,3), ai(3,3)
    DO i = 1, 3
       il = 1
       IF (i==1) il = 2
       DO j = 1, 3
          ai(j,i) = (-1.0_8)**(i+j)*det*(a(il,jl)*a(iu,ju)-a(il,ju)*a(iu,jl))
       END DO
    END DO
  END SUBROUTINE calbrec
