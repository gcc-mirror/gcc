! { dg-lto-do link }
! { dg-lto-options {{-flto -g -fPIC -shared} {-O -flto -g -fPIC -shared}} }

      FUNCTION makenumberstring(x)
      IMPLICIT NONE
      REAL, INTENT(IN)     :: x
      CHARACTER(len=20)      :: makenumberstring
      INTEGER             :: xx
      xx = x**2  ! << ICE
      makenumberstring = ''
      END FUNCTION

