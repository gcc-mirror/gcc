! { dg-do compile }
! PR 118845 - reduced from a segfault in Lapack.
SUBROUTINE SDRVES(  RESULT )
  external SSLECT
  CALL SGEES( SSLECT )
  CALL SGEES( SSLECT )
  RESULT = SSLECT( 1, 2 )
END
