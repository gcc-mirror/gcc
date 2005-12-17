c { dg-do compile }
      REAL(kind=8) A,B,C
      REAL(kind=4) RARRAY(19)
      DATA RARRAY /19*-1/
      INTEGER BOTTOM,RIGHT
      INTEGER IARRAY(19)
      DATA IARRAY /0,0,0,0,0,0,0,0,0,0,0,0,13,14,0,0,0,0,0/
      EQUIVALENCE (RARRAY(13),BOTTOM),(RARRAY(14),RIGHT)
C
      IF(I.NE.0) call exit(1)
C gcc: Internal compiler error: program f771 got fatal signal 11
C  at this point!
      END

! previously g77.ftorture/compile/alpha1.f with following alpha1.x
!
!# This test fails compilation in cross-endian environments, for example as
!# below, with a "sorry" message.
!
!if { [ishost "i\[34567\]86-*-*"] } {
!    if { [istarget "mmix-knuth-mmixware"]
! || [istarget "powerpc-*-*"] } {
!    set torture_compile_xfail [istarget]
!    }
!}
!
!return 0
