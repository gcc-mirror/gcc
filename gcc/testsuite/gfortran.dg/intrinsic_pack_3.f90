! { dg-do run }
! { dg-require-effective-target fortran_large_int }
! Take the pack intrinsic through its paces, with all types that are
! normally accessible.
program main
  implicit none
  integer,parameter :: k = selected_int_kind (range (0_8) + 1)
  integer :: i
  integer(kind=k), dimension(3,3) :: ik
  integer(kind=k), dimension(9) :: vik
  integer(kind=k), dimension(9) :: rik

  vik = (/(i+10,i=1,9)/)
  ik = reshape((/1_k, -1_k, 2_k, -2_k, 3_k, -3_k, 4_k, -4_k, 5_k/), shape(ik))
  rik = pack(ik,ik>0,vik)
  if (any(rik /= (/1_k, 2_k, 3_k, 4_k, 5_k, 16_k, 17_k, 18_k, 19_k/))) &
       & call abort


end program main
