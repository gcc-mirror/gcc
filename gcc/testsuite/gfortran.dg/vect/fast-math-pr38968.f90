! Skip this on platforms that don't have the vectorization instructions
! to handle complex types.  This test is very slow on these platforms so
! skipping is better then running it unvectorized.
! { dg-skip-if "" { ia64-*-* sparc*-*-* } { "*" } { "" } }
program mymatmul
  implicit none
  integer, parameter :: kp = 4
  integer, parameter :: n = 2000
  real(kp), dimension(n,n) :: rr, ri
  complex(kp), dimension(n,n) :: a,b,c
  real :: t1, t2
  integer :: i, j, k

  do j = 1, n
     do k = 1, n
        do i = 1, n
           c(i,j) = c(i,j) + a(i,k) * b(k,j)
        end do
     end do
  end do

end program mymatmul

! { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } }
! { dg-final { cleanup-tree-dump "vect" } }
