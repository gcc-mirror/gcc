! { dg-do run }
! { dg-additional-options "-fno-inline -fno-ipa-modref -fdump-tree-optimized -fdump-tree-original" }
!
! PR fortran/105012
! Check that the INTENT(OUT) attribute causes one clobber to be emitted in
! the caller before the call to Y in the *.original dump, and the
! initialization constant to be optimized away in the *.optimized dump,
! despite the non-explicit interface if the subroutine with the INTENT(OUT)
! is declared in the same file.

SUBROUTINE Y (Z)
      integer, intent(out) :: Z
      Z = 42
END SUBROUTINE Y
PROGRAM TEST
    integer :: X
    X = 123456789
    CALL Y (X)
    if (X.ne.42) STOP 1
END PROGRAM

! { dg-final { scan-tree-dump-times "CLOBBER" 1 "original" } }
! { dg-final { scan-tree-dump "x = {CLOBBER};" "original" } }
! { dg-final { scan-tree-dump-not "123456789" "optimized" { target __OPTIMIZE__ } } }
