! { dg-do compile }

integer :: i, j, k
integer :: x(5), y(2,5)

!$omp parallel do private(i)
do i = 1, 5
  x(i) = 42
end do

!$omp parallel do lastprivate(i)
do i = 1, 5
  x(i) = 42
end do


!$omp simd private(i)
do i = 1, 5
  x(i) = 42
end do

!$omp simd linear(i)
do i = 1, 5
  x(i) = 42
end do

!$omp simd lastprivate(i)
do i = 1, 5
  x(i) = 42
end do


!$omp simd private(i) lastprivate(j) collapse(2)
do i = 1, 5
  do j = 1, 2
    y(j, i) = 52
  end do
end do

!$omp simd lastprivate(i) private(j) collapse(2)
do i = 1, 5
  do j = 1, 2
    y(j, i) = 52
  end do
end do

!$omp parallel do firstprivate(i)
do i = 1, 5  ! { dg-error "PARALLEL DO iteration variable present on clause other than PRIVATE or LASTPRIVATE" }
  x(i) = 42
end do

!$omp parallel do simd firstprivate(i)
do i = 1, 5  ! { dg-error "PARALLEL DO SIMD iteration variable present on clause other than PRIVATE, LASTPRIVATE or LINEAR" }
  x(i) = 42
end do

!$omp simd linear(i) collapse(2)
do i = 1, 5  ! { dg-error "SIMD iteration variable present on clause other than PRIVATE or LASTPRIVATE" }
  do j = 1, 2
    y(j, i) = 52
  end do
end do


end
