  ! { dg-do  run }
  ! { dg-options "-ffrontend-optimize" }
  ! PR 81974 - this used to cause an ICE.
  
  implicit none
      COMPLEX(kind=kind(0d0)), DIMENSION(3, 3) :: R
      REAL(kind=kind(0d0)), DIMENSION(3, 3)    :: M,a,b
      complex(8), dimension(3,3) :: res, c
      integer :: i, j, k
      c = 0
      call random_number(m)
      call random_number(a)
      call random_number(b)
      r = cmplx(a, b, 8)
      do k=1,3
         do j=1,3
            do i=1,3
               c(k,j) = c(k,j) + conjg(r(i,k)) * m(i,j)
            end do
         end do
      end do
      res = MATMUL(TRANSPOSE(CONJG(R)), M)
      if (any(abs(res-c) >= 1e-6)) call abort
      c = 0
      do k=1,3
         do j=1,3
            do i=1,3
               c(i,k) = c(i,k) + m(i,j) * conjg(r(k,j))
            end do
         end do
      end do
      res = matmul(m, transpose(conjg(r)))
      if (any(abs(res-c) >= 1e-6)) call abort
      END
