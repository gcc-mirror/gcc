! { dg-do run }
! { dg-options "-fcheck=bits -fdump-tree-original" }
! { dg-shouldfail "Fortran runtime error: FROMPOS(64)+LEN(1)>BIT_SIZE(64) in intrinsic MVBITS" }
! { dg-output "At line 33 .*" }
!
! Verify that the runtime checks for the MVBITS intrinsic functions
! do not generate false-positives
program check
  implicit none
  integer, parameter :: bs4 = bit_size (1_4)
  integer, parameter :: bs8 = bit_size (1_8)
  integer(4), dimension(0:bs4) :: from4, frompos4, len4, to4, topos4
  integer(8), dimension(0:bs8) :: from8, frompos8, len8, to8, topos8
  integer :: i
  from4 = -1
  to4 = -1
  len4 = [ (i, i=0,bs4) ]
  frompos4 = bs4 - len4
  topos4 = frompos4
  call mvbits (from4, frompos4, len4, to4, topos4)
  if (any (to4 /= -1)) stop 1
  from8 = -1
  to8 = -1
  len8 = [ (i, i=0,bs8) ]
  frompos8 = bs8 - len8
  topos8 = frompos8
  call mvbits (from8, frompos8, len8, to8, topos8)
  if (any (to8 /= -1)) stop 2
  from8 = -1
  to8 = -1
  len8(0) = 1
  ! The following line should fail with a runtime error:
  call mvbits (from8, frompos8, len8, to8, topos8)
  ! Should never get here with -fcheck=bits
  stop 3
end

! { dg-final { scan-tree-dump-times "_gfortran_runtime_error_at" 15 "original" } }
