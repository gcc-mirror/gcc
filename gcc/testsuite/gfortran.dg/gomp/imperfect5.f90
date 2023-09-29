! This test case is expected to fail due to errors.

module mm

implicit none
integer, parameter :: N = 30
integer, parameter :: M = 3

integer :: a(M,N), b(M,N), c(M,N)

contains

subroutine dostuff (index, flag)
  integer :: index, flag
end subroutine

! These functions should compile without error.
subroutine good1 ()
  integer :: i, j, x, shift
  
  x = 0
  !$omp parallel do simd collapse(2) reduction(inscan,+: x) private(shift)
  do i = 1, N
    do j = 1, M
      x = x + a(j,i)
      x = x + b(j,i)
      !$omp scan inclusive(x)
      shift = i + 29*j
      c(j,i) = x + shift;
    end do
  end do
end subroutine

subroutine good2 ()
  integer :: i, j, x, shift
  
  x = 0
  !$omp parallel do simd collapse(2) reduction(inscan,+: x) private(shift)
  do i = 1, N
    do j = 1, M
      shift = i + 29*j
      c(j,i) = x + shift;
      !$omp scan exclusive(x)
      x = x + a(j,i)
      x = x + b(j,i)
    end do
  end do
end subroutine

! Adding intervening code should trigger an error.
subroutine bad1 ()
  integer :: i, j, x, shift
  
  x = 0
  !$omp parallel do simd collapse(2) reduction(inscan,+: x) private(shift) ! { dg-error "inner loops must be perfectly nested" }
  do i = 1, N
    call dostuff (i, 0)
    do j = 1, M
      x = x + a(j,i)
      x = x + b(j,i)
      !$omp scan inclusive(x)
      shift = i + 29*j
      c(j,i) = x + shift;
    end do
  end do
end subroutine

subroutine bad2 ()
  integer :: i, j, x, shift
  
  x = 0
  !$omp parallel do simd collapse(2) reduction(inscan,+: x) private(shift) ! { dg-error "inner loops must be perfectly nested" }
  do i = 1, N
    do j = 1, M
      shift = i + 29*j
      c(j,i) = x + shift;
      !$omp scan exclusive(x)
      x = x + a(j,i)
      x = x + b(j,i)
    end do
    call dostuff (i, 1)
  end do
end subroutine

end module