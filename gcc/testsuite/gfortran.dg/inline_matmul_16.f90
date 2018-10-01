! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-optimized -Wrealloc-lhs -finline-matmul-limit=1000 -O" }
! PR 66094: Check functionality for MATMUL(TRANSPOSE(A),B)) for two-dimensional arrays
program main
  implicit none
  integer, parameter :: n = 3, m=4, cnt=2
  real, dimension(cnt,n) :: a
  real, dimension(cnt,m) :: b
  real, dimension(n,m) :: c, cres
  real, dimension(:,:), allocatable :: calloc
  integer :: in, im, icnt

  data a / 2., -3., 5., -7., 11., -13./
  data b /17., -23., 29., -31., 37., -39., 41., -47./
  data cres /103.,  246.,  486.,  151.,  362.,  722., &
             191.,  458.,  914.,  223.,  534., 1062./

  c = matmul(transpose(a),b)
  if (sum(c-cres)>1e-4) STOP 1
  if (sum(c-cres)>1e-4) STOP 2

  ! Unallocated
  calloc = matmul(transpose(a),b) ! { dg-warning "Code for reallocating the allocatable array" }
  if (any(shape(c) /= shape(calloc))) STOP 3
  if (sum(calloc-cres)>1e-4) STOP 4
  deallocate(calloc)

  ! Allocated to wrong shape
  allocate (calloc(10,10))
  calloc = matmul(transpose(a),b) ! { dg-warning "Code for reallocating the allocatable array" }
  if (any(shape(c) /= shape(calloc))) STOP 5
  if (sum(calloc-cres)>1e-4) STOP 6
  deallocate(calloc)

  ! cycle through a few test cases...
  do in=2,10 
     do im = 2,10
        do icnt = 2,10
           block
             real, dimension(icnt,in) :: a2
             real, dimension(icnt,im) :: b2
             real, dimension(in,im) :: c2,cr
             integer :: i,j,k
             call random_number(a2)
             call random_number(b2)
             c2 = 0
             do i=1,size(a2,2)
                do j=1, size(b2,2)
                   do k=1, size(a2,1)
                      c2(i,j) = c2(i,j) + a2(k,i) * b2(k,j)
                   end do
                end do
             end do
             cr = matmul(transpose(a2), b2)
             if (any(abs(c2-cr) > 1e-4)) STOP 7
           end block
        end do
     end do
  end do
end program main
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 1 "optimized" } }
