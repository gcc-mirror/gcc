! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 37131 - check rank1/rank2 and rank2/rank1 cases for inline matmul.

module foo
  implicit none
contains
  subroutine a1b2(a,b,c)
    real, dimension(:), intent(in) :: a
    real, dimension(:,:), intent(in) :: b
    real, dimension(:), intent(out) :: c

    c = matmul(a,b)
  end subroutine a1b2

  subroutine a2b1(a,b,c)
    real, dimension(:,:), intent(in) :: a
    real, dimension(:), intent(in) :: b
    real, dimension(:), intent(out) :: c

    c = matmul(a,b)
  end subroutine a2b1
end module foo

program main
  use foo
  implicit none
  real, dimension(3) :: a1
  real, dimension(3,2) :: b1
  real, dimension(2) :: c1

  real, dimension(3,2) :: a2
  real, dimension(2) :: b2
  real, dimension(3) :: c2

  data a1 /17., -23., 29./
  data b1 / 2.,  -3.,  5.,  -7., 11., -13./

  data b2/-2.,5./

  a2 = -b1
  call a1b2(a1,b1,c1)
  if (any(abs(c1 - (/248., -749./)) > 1e-3)) call abort
  call a2b1(a2,b2,c2)
  if (any(abs(c2 - (/39., -61., 75./)) > 1e-3)) call abort
end program main

! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "original" } }
