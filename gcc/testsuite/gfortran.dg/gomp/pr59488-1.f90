! PR fortran/59488
! { dg-do compile }
! { dg-options "-fopenmp" }

  implicit none
  integer, parameter :: p(2) = (/ 11, 12 /)
  integer :: r

  !$omp parallel do default(none)
  do r = 1, 2
    print *, p(r)
  end do
end
