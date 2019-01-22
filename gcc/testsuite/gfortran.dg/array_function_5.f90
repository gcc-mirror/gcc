! { dg-do run }
! PR41278 internal compiler error related to matmul and transpose
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
! Original test case by Chris <cmklaij@hetnet.nl>
program bug
  implicit none
  real, dimension(3,3) :: matA,matB,matC

  matA(1,:)=(/1., 2., 3./)
  matA(2,:)=(/4., 5., 6./)
  matA(3,:)=(/7., 8., 9./)

  matB=matmul(transpose(0.5*matA),matA)
  matC = transpose(0.5*matA)
  matC = matmul(matC, matA)
  if (any(matB.ne.matC)) STOP 1
end program bug
