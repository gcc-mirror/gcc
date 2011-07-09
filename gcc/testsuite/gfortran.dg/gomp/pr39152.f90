! { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
! { dg-options "-march=i486 -fopenmp -mavx -O3 -funroll-all-loops" } 

  call test_workshare

contains
  subroutine test_workshare
    integer :: i, j, k, l, m
    double precision, dimension (64) :: d, e
    integer, dimension (10) :: f, g
    integer, dimension (16, 16) :: a, b, c
    integer, dimension (16) :: n
!$omp parallel num_threads (4) private (j, k)
!$omp barrier
!$omp workshare
    where (g .lt. 0)
      f = 100
    elsewhere
      where (g .gt. 6) f = f + sum (g)
      f = 300 + f
    end where
!$omp end workshare nowait
!$omp workshare
    forall (j = 1:16, k = 1:16) b (k, j) = a (j, k)
    forall (j = 2:16, n (17 - j) / 4 * 4 .ne. n (17 - j))
      n (j) = n (j - 1) * n (j)
    end forall
!$omp endworkshare
!$omp end parallel

  end subroutine test_workshare
end
