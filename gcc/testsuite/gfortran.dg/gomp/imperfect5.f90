! This test case is expected to fail due to errors.

subroutine f1 (depth, iter)
  integer :: depth, iter
end subroutine

subroutine f2 (depth, iter)
  integer :: depth, iter
end subroutine

function ijk (x, y, z)
  integer :: ijk
  integer :: x, y, z
end function

subroutine f3 (sum)
  integer :: sum
end subroutine  

! This function isn't particularly meaningful, but it should compile without
!  error.
function s1 (a1, a2, a3)
  integer :: s1
  integer :: a1, a2, a3
  integer :: i, j, k
  integer :: r
  
  r = 0
  !$omp simd collapse(3) reduction (inscan, +:r)
  do i = 1, a1
    do j = 1, a2
      do k = 1, a3
        r = r + ijk (i, j, k)
!$omp scan exclusive (r)
        call f3 (r)
      end do
    end do
  end do

  s1 = r
end function

! Adding intervening code should trigger an error.
function s2 (a1, a2, a3)
  integer :: s2
  integer :: a1, a2, a3
  integer :: i, j, k
  integer :: r
  
  r = 0
  !$omp simd collapse(3) reduction (inscan, +:r)     ! { dg-error "inner loops must be perfectly nested" }
  do i = 1, a1
    call f1 (1, i)
    do j = 1, a2
      call f1 (2, j)
      do k = 1, a3
        r = r + ijk (i, j, k)
!$omp scan exclusive (r)
        call f3 (r)
      end do
      call f2 (2, j)
    end do
    call f2 (1, i)
  end do
  
  s2 = r
end function
