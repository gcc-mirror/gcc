! { dg-do run }
! { dg-additional-options "-ffrontend-optimize -fdump-tree-original" }

PROGRAM memain
  implicit none
  REAL(8), DIMENSION(3) :: b
  REAL(8), DIMENSION(3,2) :: A
  real(8), dimension(:), allocatable :: y
  real(8), dimension(2) :: z
  integer :: i, j

  A(:,1) = [1,-2,3]
  A(:,2) = [-4,5,6]
    
  b =  [7,-8,9]
  y = matmul(transpose(A),b)
  z = 0
  do i=1,2
    do j=1,3
       z(i) = z(i) + a(j,i) * b(j)
     end do
  end do
  if (size(z,1) /= size(y,1)) stop 1
  if (any(abs(z - y) > 1e-12)) stop 2
    
END PROGRAM memain
! { dg-final { scan-tree-dump-not "_gfortran_matmul_r8" "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_transpose_r8" "original" } }
