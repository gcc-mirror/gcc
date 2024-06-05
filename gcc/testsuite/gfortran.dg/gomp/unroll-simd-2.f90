! { dg-do run }
! { dg-options "-O2 -fopenmp-simd -fdump-tree-original -fdump-tree-gimple" }

module test_functions
  contains
  integer function compute_sum() result(sum)
    implicit none

    integer :: i,j

    !$omp simd
    do i = 1,10,3
      !$omp unroll full
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function

  integer function compute_sum2() result(sum)
    implicit none

    integer :: i,j

    !$omp simd
    !$omp unroll partial(2)
    do i = 1,10,3
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function
end module test_functions

program test
  use test_functions
  implicit none

  integer :: result

  result = compute_sum ()
  write (*,*) result
  if (result .ne. 16) then
    call abort
  end if

  result = compute_sum2 ()
  write (*,*) result
  if (result .ne. 16) then
    call abort
  end if
end program

! { dg-final { scan-tree-dump "omp unroll full" "original" } }
! { dg-final { scan-tree-dump "omp unroll partial\\\(2\\\)" "original" } }
! { dg-final { scan-tree-dump-not "omp unroll" "gimple" } }
