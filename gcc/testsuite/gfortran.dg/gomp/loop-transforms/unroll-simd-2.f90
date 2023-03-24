! { dg-do run }
! { dg-options "-O2 -fopenmp-simd" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

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

! { dg-final { scan-tree-dump {omp loop_transform} "original" } }
! { dg-final { scan-tree-dump-not {omp loop_transform} "omp_transform_loops" } }
