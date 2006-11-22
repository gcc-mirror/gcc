! { dg-do compile }
! Tests the fix for PR25087, in which the following invalid code
! was not detected.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
SUBROUTINE s(n)
  CHARACTER(LEN=n), EXTERNAL :: a ! { dg-error "must have an explicit interface" }
  interface
    function b (m)                ! This is OK
      CHARACTER(LEN=m) :: b
      integer :: m
    end function b
  end interface
  write(6,*) a(n)
  write(6,*) b(n)
  write(6,*) c()
contains
    function c ()                ! This is OK
      CHARACTER(LEN=n):: c
      c = ""
    end function c
END SUBROUTINE s

