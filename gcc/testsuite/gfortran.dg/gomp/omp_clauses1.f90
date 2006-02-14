! { dg-do compile }
      subroutine test1
	integer :: i, j, k, l
	common /b/ j, k
!$omp parallel shared (i) private (/b/)
!$omp end parallel
!$omp parallel do shared (/b/), firstprivate (i), lastprivate (i)
	do l = 1, 10
	end do
!$omp end parallel do
!$omp parallel shared (j) private (/b/) ! { dg-error "'j' present on multiple clauses" }
!$omp end parallel
!$omp parallel shared (j, j) private (i) ! { dg-error "'j' present on multiple clauses" }
!$omp end parallel
!$omp parallel firstprivate (i, j, i) ! { dg-error "'i' present on multiple clauses" }
!$omp end parallel
!$omp parallel shared (i) private (/b/, /b/) ! { dg-error "'\[jk\]' present on multiple clauses" }
!$omp end parallel
!$omp parallel shared (i) reduction (+ : i, j) ! { dg-error "'i' present on multiple clauses" }
!$omp end parallel
!$omp parallel do shared (/b/), firstprivate (/b/), lastprivate (i) ! { dg-error "'\[jk\]' present on multiple clauses" }
	do l = 1, 10
	end do
!$omp end parallel do
      end subroutine test1
