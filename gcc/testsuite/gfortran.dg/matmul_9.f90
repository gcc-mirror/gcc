! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/56318
!
! Contributed by Alberto Luaces
!
SUBROUTINE mass_matrix        
  DOUBLE PRECISION,PARAMETER::m1=1.d0
  DOUBLE PRECISION,DIMENSION(3,2),PARAMETER::A1=reshape([1.d0,0.d0, 0.d0, &
       0.d0,1.d0, 0.d0],[3,2])
  DOUBLE PRECISION,DIMENSION(2,2),PARAMETER::Mel=reshape([1.d0/3.d0, 0.d0, &
       0.d0, 1.d0/3.d0],[2,2])

  DOUBLE PRECISION,DIMENSION(3,3)::MM1

  MM1=m1*matmul(A1,matmul(Mel,transpose(A1)))
  !print '(3f8.3)', MM1
  if (any (abs (MM1 &
                - reshape ([1.d0/3.d0, 0.d0,      0.d0,  &
                            0.d0,      1.d0/3.d0, 0.d0,  &
                            0.d0,      0.d0,      0.d0], &
                           [3,3])) > epsilon(1.0d0))) &
    call abort ()
END SUBROUTINE mass_matrix

program name
  implicit none
  integer, parameter :: A(3,2) = reshape([1,2,3,4,5,6],[3,2])
  integer, parameter :: B(2,3) = reshape([3,17,23,31,43,71],[2,3])
  integer, parameter :: C(3)   = [-5,-7,-21]
  integer, parameter :: m1 = 1

!  print *, matmul(B,C)
   if (any (matmul(B,C) /= [-1079, -1793])) call abort()
!  print *, matmul(C,A)
   if (any (matmul(C,A) /= [-82, -181])) call abort()
!  print '(3i5)', m1*matmul(A,B)
  if (any (m1*matmul(A,B) /= reshape([71,91,111, 147,201,255, 327,441,555],&
                                     [3,3]))) &
     call abort()
  call mass_matrix
end program name

! { dg-final { scan-tree-dump-times "matmul" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }

