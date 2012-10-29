 SUBROUTINE calc_S_derivs()
    INTEGER, DIMENSION(6, 2)      :: c_map_mat
    INTEGER, DIMENSION(:), POINTER:: C_mat
    DO j=1,3
       DO m=j,3
          n=n+1
          c_map_mat(n,1)=j
          IF(m==j)CYCLE
          c_map_mat(n,2)=m
       END DO
    END DO
    DO m=1,6
       DO j=1,2
          IF(c_map_mat(m,j)==0)CYCLE
          CALL foo(C_mat(c_map_mat(m,j))) 
       END DO
    END DO
  END SUBROUTINE calc_S_derivs
