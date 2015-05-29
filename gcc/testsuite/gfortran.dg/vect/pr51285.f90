! { dg-do compile }

   SUBROUTINE smm_dnn_4_10_10_1_1_2_1(A,B,C)
      REAL   :: C(4,10), B(10,10), A(4,10)
      DO j=           1 ,          10 ,           2
      DO i=           1 ,           4 ,           1
      DO l=           1 ,          10 ,           1
        C(i+0,j+0)=C(i+0,j+0)+A(i+0,l+0)*B(l+0,j+0)
        C(i+0,j+1)=C(i+0,j+1)+A(i+0,l+0)*B(l+0,j+1)
      ENDDO
      ENDDO
      ENDDO
    END SUBROUTINE
   SUBROUTINE smm_dnn_4_10_10_6_4_1_1(A,B,C)
      REAL   :: C(4,10), B(10,10), A(4,10)
      DO l=           1 ,          10 ,           1
      DO j=           1 ,          10 ,           1
        C(i+0,j+0)=C(i+0,j+0)+A(i+0,l+0)*B(l+0,j+0)
      ENDDO
      ENDDO
    END SUBROUTINE
 SUBROUTINE S(A,B,C)
    INTEGER :: Nmin=2,Niter=100
    REAL, DIMENSION(:,:), ALLOCATABLE   :: A,B,C
    DO imin=1,Nmin
     DO i=1,Niter
       CALL smm_dnn_4_10_10_1_1_2_1(A,B,C)
     ENDDO
     DO i=1,Niter
       CALL smm_dnn_4_10_10_6_4_1_1(A,B,C)
     ENDDO
     CALL foo()
    ENDDO
 END SUBROUTINE

