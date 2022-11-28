! Cross check that it is accepted without nowait
subroutine bar()
implicit none
integer :: i, a(5)
!$omp atomic write
i = 5
!$omp end atomic

!$omp critical
!$omp end critical

!$omp distribute
do i = 1, 5
end do
!$omp end distribute

!$omp distribute parallel do
do i = 1, 5
end do
!$omp end distribute parallel do

!$omp distribute parallel do simd
do i = 1, 5
end do
!$omp end distribute parallel do simd

!$omp distribute simd
do i = 1, 5
end do
!$omp end distribute simd

!$omp masked
!$omp end masked

!$omp masked taskloop
do i = 1, 5
end do
!$omp end masked taskloop

!$omp masked taskloop simd
do i = 1, 5
end do
!$omp end masked taskloop simd

!$omp master
!$omp end master

!$omp master taskloop
do i = 1, 5
end do
!$omp end master taskloop

!$omp master taskloop simd
do i = 1, 5
end do
!$omp end master taskloop simd

!$omp ordered
!$omp end ordered

!$omp parallel
!$omp end parallel

!$omp parallel workshare
a(:) = 5
!$omp end parallel workshare

!$omp parallel do
do i = 1, 5
end do
!$omp end parallel do

!$omp parallel do simd
do i = 1, 5
end do
!$omp end parallel do simd

!$omp parallel sections
  !$omp section
  block; end block
!$omp end parallel sections

!$omp parallel masked
!$omp end parallel masked

!$omp parallel masked taskloop
do i = 1, 5
end do
!$omp end parallel masked taskloop

!$omp parallel masked taskloop simd
do i = 1, 5
end do
!$omp end parallel masked taskloop simd

!$omp parallel master
!$omp end parallel master

!$omp parallel master taskloop
do i = 1, 5
end do
!$omp end parallel master taskloop

!$omp parallel master taskloop simd
do i = 1, 5
end do
!$omp end parallel master taskloop simd

!$omp simd
do i = 1, 5
end do
!$omp end simd

!$omp task
!$omp end task

!$omp taskgroup
!$omp end taskgroup

!$omp taskloop
do i = 1, 5
end do
!$omp end taskloop

!$omp taskloop simd
do i = 1, 5
end do
!$omp end taskloop simd

!$omp teams
!$omp end teams

!$omp teams distribute
do i = 1, 5
end do
!$omp end teams distribute

!$omp teams distribute parallel do
do i = 1, 5
end do
!$omp end teams distribute parallel do

!$omp teams distribute parallel do simd
do i = 1, 5
end do
!$omp end teams distribute parallel do simd

!$omp teams distribute simd
do i = 1, 5
end do
!$omp end teams distribute simd

!$omp target data map(tofrom:i)
!$omp end target data

end
