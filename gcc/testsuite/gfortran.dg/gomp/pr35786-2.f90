! PR fortran/35786
! { dg-do compile }
! { dg-options "-fopenmp" }

function fn7 ()
  integer :: fn7
  !$omp parallel private (fn7)
    fn7 = 6
  !$omp end parallel
  fn7 = 7
end function fn7
function fn8 ()
  integer :: fn8
  call fn9
contains
  subroutine fn9
    !$omp parallel private (fn8)
      fn8 = 6
    !$omp end parallel
    fn8 = 7
  end subroutine fn9
end function fn8
function fn10 ()
  integer :: fn10, fn11
  entry fn11 ()
  !$omp parallel private (fn10)
    fn10 = 6
  !$omp end parallel
  !$omp parallel private (fn11)
    fn11 = 6
  !$omp end parallel
  fn10 = 7
end function fn10
function fn12 ()
  integer :: fn12, fn13
  entry fn13 ()
  call fn14
contains
  subroutine fn14
    !$omp parallel private (fn12)
      fn12 = 6
    !$omp end parallel
    !$omp parallel private (fn13)
      fn13 = 6
    !$omp end parallel
    fn12 = 7
  end subroutine fn14
end function fn12
