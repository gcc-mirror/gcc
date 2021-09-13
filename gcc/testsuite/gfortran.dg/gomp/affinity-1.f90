  integer :: i, j
  integer, dimension (10, 10) :: a
!$omp parallel do default(none)proc_bind(master)shared(a)
  do i = 1, 10
    j = 4
    do j = 1, 10
      a(i, j) = i + j
    end do
    j = 8
  end do
!$omp end parallel do
!$omp parallel do default(none)proc_bind(primary)shared(a)
  do i = 1, 10
    j = 4
    do j = 1, 10
      a(i, j) = i + j
    end do
    j = 8
  end do
!$omp end parallel do
!$omp parallel proc_bind (close)
!$omp parallel default(none) proc_bind (spread) firstprivate(a) private (i)
  do i = 1, 10
    a(i, i) = i
  enddo
!$omp end parallel
!$omp endparallel
end
