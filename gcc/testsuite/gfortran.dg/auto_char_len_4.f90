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
  CHARACTER(LEN=n), EXTERNAL :: a  ! { dg-error "declared with a constant character length" }
  CHARACTER(LEN=n), EXTERNAL :: d  ! { dg-error "declared with a constant character length" }
  interface
    function b (m)                ! This is OK
      integer :: m
      CHARACTER(LEN=m) :: b
    end function b
    function e (m)              ! { dg-warning "Possible character length mismatch" }
      integer :: m
      CHARACTER(LEN=m) :: e
    end function e
  end interface
  write(6,*) a()
  write(6,*) b(n)
  write(6,*) c()
  write(6,*) d()
  write(6,*) e(n)
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

function e(k)                   ! { dg-warning "Possible character length mismatch" }
  integer :: k
  character(len=k+1-1) :: e
  e = ''
end function e
