! { dg-do run }
! { dg-additional-options "-fdump-tree-original -std=f2018" }
! Check that the QUIET specifier to shut up a STOP statement is passed properly

program p
  logical(1) :: q = .true.  ! using kind=1 to simplify scanning of tree dump
  stop 0, quiet=q
  stop 1, quiet=.true.
  stop 2                    ! the "noisy" default
end program p

! { dg-final { scan-tree-dump "_gfortran_stop_numeric \\(0, q\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_stop_numeric \\(1, 1\\)" "original" } }
! { dg-final { scan-tree-dump "_gfortran_stop_numeric \\(2, 0\\)" "original" } }
