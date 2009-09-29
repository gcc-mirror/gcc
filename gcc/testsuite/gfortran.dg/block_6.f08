! { dg-do run { xfail *-*-* } }
! { dg-options "-std=f2008 -fall-intrinsics" }

! Check for correct scope of variables that are implicit typed within a BLOCK.
! This is not yet implemented, thus XFAIL'ed the test.

PROGRAM main
  IMPLICIT INTEGER(a-z)

  BLOCK
    ! a gets implicitly typed, but scope should not be limited to BLOCK.
    a = 42
  END BLOCK

  ! Here, we should still access the same a that was set above.
  IF (a /= 42) CALL abort ()
END PROGRAM main
