! { dg-do compile }
! Tests the fix for PR32875, in which the character length for the
! array constructor would get lost in simplification and would lead
! the error 'Not Implemented: complex character array constructor'.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  call foo ((/(S1(i),i=1,3,-1)/))
CONTAINS
  FUNCTION S1(i)
    CHARACTER(LEN=1) :: S1
    INTEGER :: I
    S1="123456789"(i:i)
  END FUNCTION S1
  subroutine foo (chr)
    character(1) :: chr(:)
    print *, chr
  end subroutine
END
