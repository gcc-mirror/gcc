! { dg-do run }
! { dg-require-effective-target fortran_large_int }
! Test that the internal pack and unpack routines work OK
! for our large integer type.

program main
  integer,parameter :: k = selected_int_kind (range (0_8) + 1)
  integer(kind=k), dimension(3) :: ik

  ik = (/ -1, 1, -3 /)
  call sub_ik(ik(1:3:2))
  if (any(ik /= (/ 3, 1, 2 /))) call abort
end program main

subroutine sub_ik(i)
  integer,parameter :: k = selected_int_kind (range (0_8) + 1)
  integer(kind=k), dimension(2) :: i
  if (i(1) /= -1) call abort
  if (i(2) /= -3) call abort
  i(1) = 3
  i(2) = 2
end subroutine sub_ik
