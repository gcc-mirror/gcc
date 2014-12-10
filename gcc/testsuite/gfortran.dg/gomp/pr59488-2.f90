! PR fortran/59488
! { dg-do compile }
! { dg-options "-fopenmp" }

  implicit none
  type t
    integer :: s1, s2, s3
  end type
  integer :: r
  type(t), parameter :: u = t(1, 2, 3)

  !$omp parallel do default(none)
  do r = 1, 2
    print *, u
  end do
end
