! { dg-lto-do link }
! { dg-lto-options {{-flto -g -fPIC -r -nostdlib} {-O -flto -g -fPIC -r -nostdlib}} }
! { dg-extra-ld-options "-flinker-output=nolto-rel" }

      FUNCTION makenumberstring(x)
      IMPLICIT NONE
      REAL, INTENT(IN)     :: x
      CHARACTER(len=20)      :: makenumberstring
      INTEGER             :: xx
      xx = x**2  ! << ICE
      makenumberstring = ''
      END FUNCTION

