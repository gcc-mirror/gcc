! PR middle-end/104757
! { dg-do compile }
! { dg-options "-O -fopenmp" }

module pr104757
  implicit none (external, type)
  integer :: ll
  !$omp declare target (ll)
contains
  subroutine foo (i1)
    !$omp declare target (foo)
    logical :: i1
    integer :: i
    !$omp distribute simd if(i1)
    do i = 1, 64
      ll = ll + 1
    end do
  end
end module
