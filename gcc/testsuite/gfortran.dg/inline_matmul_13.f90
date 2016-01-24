! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original -Wrealloc-lhs" }
! PR 66094: Check functionality for MATMUL(A, TRANSPSE(B))
module x
contains
  subroutine mm1(a,b,c)
    real, dimension(:,:), intent(in) :: a, b
    real, dimension(:,:), intent(out) :: c
    c = -42.
    c = matmul(a, transpose(b))
  end subroutine mm1
end module x

program main
  use x
  implicit none
  integer, parameter :: n = 3, m=4, cnt=2
  real, dimension(n,cnt) :: a
  real, dimension(m,cnt) :: b
  real, dimension(n,m) :: c, cres
  real, dimension(:,:), allocatable :: calloc

  data a / 2., -3., 5., -7., 11., -13./
  data b /17., -23., 29., -31., 37., -39., 41., -47./
  data cres / -225., 356., -396., 227., -360., 392., &
       -229., 364., -388., 267., -424., 456./ 

  c = matmul(a,transpose(b))
  if (sum(c-cres)>1e-4) call abort
  call mm1 (a, b, c)
  if (sum(c-cres)>1e-4) call abort

  ! Unallocated
  calloc = matmul(a,transpose(b)) ! { dg-warning "Code for reallocating the allocatable array" }
  if (any(shape(c) /= shape(calloc))) call abort
  if (sum(calloc-cres)>1e-4) call abort
  deallocate(calloc)

  ! Allocated to wrong shape
  allocate (calloc(10,10))
  calloc = matmul(a,transpose(b)) ! { dg-warning "Code for reallocating the allocatable array" }
  if (any(shape(c) /= shape(calloc))) call abort
  if (sum(calloc-cres)>1e-4) call abort
  deallocate(calloc)

end program main
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "original" } }
