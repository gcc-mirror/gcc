! { dg-do compile }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 77915 - ICE of matmul with forall.
program x
  integer, parameter :: d = 3
  real,dimension(d,d,d) :: cube,xcube
  real, dimension(d,d) :: cmatrix
  integer :: i,j
  forall(i=1:d,j=1:d)
     xcube(i,j,:) = matmul(cmatrix,cube(i,j,:))
  end forall
end program x

! { dg-final { scan-tree-dump-times "_gfortran_matmul" 1 "original" } }
