! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine collapse1
  integer :: i, j, k, a(1:3, 4:6, 5:7)
  real :: r
  logical :: l
  integer, save :: thr
  !$omp threadprivate (thr)
  l = .false.
  a(:, :, :) = 0
  !$omp parallel do collapse(4) schedule(static, 4) ! { dg-error "not enough DO loops for collapsed" }
    do i = 1, 3
      do j = 4, 6
        do k = 5, 7
          a(i, j, k) = i + j + k
        end do
      end do
    end do
  !$omp parallel do collapse(2)
    do i = 1, 5, 2
      do j = i + 1, 7, i	! { dg-error "collapsed loops don.t form rectangular iteration space" }
      end do
    end do
  !$omp parallel do collapse(2) shared(j)
    do i = 1, 3
      do j = 4, 6		! { dg-error "iteration variable present on clause other than PRIVATE or LASTPRIVATE" }
      end do
    end do
  !$omp parallel do collapse(2)
    do i = 1, 3
      do j = 4, 6
      end do
      k = 4
    end do
  !$omp parallel do collapse(2)
    do i = 1, 3
      do			! { dg-error "cannot be a DO WHILE or DO without loop control" }
      end do
    end do
  !$omp parallel do collapse(2)
    do i = 1, 3
      do r = 4, 6		! { dg-warning "must be integer" }
      end do
    end do
end subroutine collapse1

subroutine collapse1_2
  integer :: i
  !$omp parallel do collapse(2)
    do i = -6, 6		! { dg-error "cannot be redefined inside loop beginning" }
      do i = 4, 6		! { dg-error "collapsed loops don.t form rectangular iteration space|cannot be redefined" }
      end do
    end do
end subroutine collapse1_2

! { dg-error "iteration variable must be of type integer" "integer" { target *-*-* } 43 }
