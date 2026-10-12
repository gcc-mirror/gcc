! { dg-do compile  { target skip-all-targets } }

! Used via  simdlen-scaled-5-1.f90

! PR middle-end/127723

subroutine f()
  use m
  implicit none
  integer :: i

  !$omp simd simdlen(scaled(integer,2): 8)
  do i = 1, 1
    call fbase (1); ! -> fvarInt2
  end do

  !$omp parallel
  block
  !$omp simd simdlen(scaled(integer,2): 8)
    do i = 1, 1
      call fbase (11); ! -> fvarInt2
    end do
  end block


  !$omp simd simdlen(scaled(integer): 8)
  do i = 1, 1
    call fbase (2); ! -> fvarInt0
  end do

  !$omp simd simdlen(4)
  do i = 1, 1
    call fbase (3); ! -> fvar0
  end do

  !$omp simd simdlen(8)
  do i = 1, 1
    call fbase (4); ! -> fbase
  end do
end subroutine
