! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR fortran/106557 - nesting intrinsics ibset and transfer gives wrong result

program p
  implicit none
  character(1) :: s

  write(s,'(i1)') ibset (transfer (0, 0), 0)
  if (s /= '1') stop 1
  write(s,'(i1)') ibclr (transfer (1, 0), 0)
  if (s /= '0') stop 2

  ! These shall be fully resolved at compile time:
  if (transfer   (ibset (transfer (0, 0), 0), 0) /= 1) stop 3
  if (transfer   (ibclr (transfer (1, 0), 0), 0) /= 0) stop 4
end

! { dg-final { scan-tree-dump-times "_gfortran_stop_numeric" 2 "original" } }
