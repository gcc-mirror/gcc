! { dg-do compile }
!
! Tests the fix for PR25087, in which the following invalid code
! was not detected.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
! Modified by Tobias Burnus to fix PR fortran/41235.
!
FUNCTION a()
  CHARACTER(len=10) :: a
  a = ''
END FUNCTION a

SUBROUTINE s(n)
  CHARACTER(LEN=n), EXTERNAL :: a  ! { dg-error "Character length mismatch" }
  CHARACTER(LEN=n), EXTERNAL :: d  ! { dg-error "Character length mismatch" }
  interface
    function b (m)                ! This is OK
      CHARACTER(LEN=m) :: b
      integer :: m
    end function b
  end interface
  write(6,*) a()
  write(6,*) b(n)
  write(6,*) c()
  write(6,*) d()
contains
    function c ()                ! This is OK
      CHARACTER(LEN=n):: c
      c = ""
    end function c
END SUBROUTINE s

FUNCTION d()
  CHARACTER(len=99) :: d
  d = ''
END FUNCTION d
