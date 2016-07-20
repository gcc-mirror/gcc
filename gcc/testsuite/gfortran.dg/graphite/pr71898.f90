! { dg-do compile }
! { dg-options "-floop-nest-optimize -O1" }

MODULE d3_poly
    INTEGER, PUBLIC, PARAMETER :: max_grad2=5
    INTEGER, PUBLIC, PARAMETER :: max_grad3=3
    INTEGER, PUBLIC, PARAMETER :: cached_dim2=(max_grad2+1)*(max_grad2+2)/2
    INTEGER, PUBLIC, PARAMETER :: cached_dim3=(max_grad3+1)*(max_grad3+2)*(max_grad3+3)/6
    INTEGER, SAVE, DIMENSION(3,cached_dim3) :: a_mono_exp3
    INTEGER, SAVE, DIMENSION(cached_dim2,cached_dim2) :: a_mono_mult2
    INTEGER, SAVE, DIMENSION(cached_dim3,cached_dim3) :: a_mono_mult3
    INTEGER, SAVE, DIMENSION(4,cached_dim3) :: a_mono_mult3a
CONTAINS
SUBROUTINE init_d3_poly_module()
    INTEGER                                  :: grad, i, ii, ij, j, subG
    INTEGER, DIMENSION(3)                    :: monoRes3
    DO grad=0,max_grad2
        DO i=grad,0,-1
            DO j=grad-i,0,-1
            END DO
        END DO
    END DO
    DO ii=1,cached_dim3
        DO ij=ii,cached_dim2
            a_mono_mult2(ij,ii)=a_mono_mult2(ii,ij)
        END DO
    END DO
    DO ii=1,cached_dim3
        DO ij=ii,cached_dim3
            monoRes3=a_mono_exp3(:,ii)+a_mono_exp3(:,ij)
            a_mono_mult3(ii,ij)=mono_index3(monoRes3(1),monoRes3(2),monoRes3(3))+1
            a_mono_mult3(ij,ii)=a_mono_mult3(ii,ij)
        END DO
    END DO
    DO i=1,cached_dim3
       DO j=1,4
          a_mono_mult3a(j,i)=a_mono_mult3(j,i)
       END DO
    END DO
END SUBROUTINE
PURE FUNCTION mono_index3(i,j,k) RESULT(res)
    INTEGER, INTENT(in)                      :: i, j, k
    res=grad*(grad+1)*(grad+2)/6+(sgrad)*(sgrad+1)/2+k
END FUNCTION
END MODULE d3_poly
