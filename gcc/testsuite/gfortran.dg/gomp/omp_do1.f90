! { dg-do compile }
! { dg-options "-fopenmp -std=legacy" }
subroutine foo
  integer :: i, j
  integer, dimension (30) :: a
  double precision :: d
  i = 0
!$omp do private (i)
  do 100 ! { dg-error "cannot be a DO WHILE or DO without loop control" }
    if (i .gt. 0) exit ! { dg-error "EXIT statement" }
100 i = i + 1
  i = 0
!$omp do private (i)
  do ! { dg-error "cannot be a DO WHILE or DO without loop control" }
    if (i .gt. 0) exit ! { dg-error "EXIT statement" }
    i = i + 1
  end do
  i = 0
!$omp do private (i)
  do 200 while (i .lt. 4) ! { dg-error "cannot be a DO WHILE or DO without loop control" }
200 i = i + 1
!$omp do private (i)
  do while (i .lt. 8) ! { dg-error "cannot be a DO WHILE or DO without loop control" }
    i = i + 1
  end do
!$omp do
  do 300 d = 1, 30, 6
    i = d
300 a(i) = 1
!$omp do
  do d = 1, 30, 5
    i = d
    a(i) = 2
  end do
!$omp do
  do i = 1, 30
    if (i .eq. 16) exit ! { dg-error "EXIT statement" }
  end do
!$omp do
outer: do i = 1, 30
    do j = 5, 10
      if (i .eq. 6 .and. j .eq. 7) exit outer ! { dg-error "EXIT statement" }
    end do
  end do outer
last: do i = 1, 30
!$omp parallel
    if (i .eq. 21) exit last ! { dg-error "leaving OpenMP structured block" }
!$omp end parallel
  end do last
!$omp parallel do shared (i)
  do i = 1, 30, 2 ! { dg-error "iteration variable present on clause" }
    a(i) = 5
  end do
!$omp end parallel do
end subroutine
! { dg-error "iteration variable must be of type integer" "" { target *-*-* } 27 }
! { dg-error "iteration variable must be of type integer" "" { target *-*-* } 31 }
