! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that all loops in the nest are annotated. 

subroutine f (a, b, c)
  implicit none

  real, intent (in), dimension(16,16) :: a
  real, intent (in), dimension(16,16) :: b
  real, intent (out), dimension(16,16) :: c
  
  integer :: i, j, k
  real :: t

!$acc kernels copyin(a(1:16,1:16), b(1:16,1:16)) copyout(c(1:16,1:16))

  do i = 1, 16
    do j = 1, 16
      t = 0
      do k = 1, 16
        t = t + a(i,k) * b(k,j)
      end do
      c(i,j) = t;
    end do
  end do

!$acc end kernels
end subroutine f

! { dg-final { scan-tree-dump-times "acc loop private\\(.\\) auto" 3 "original" } }
