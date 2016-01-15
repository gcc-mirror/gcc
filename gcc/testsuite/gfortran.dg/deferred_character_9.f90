! { dg-do run }
!
! Test the fix for PR64324 in which deferred length user ops
! were being mistaken as assumed length and so rejected.
!
! Contributed by Ian Harvey  <ian_harvey@bigpond.com>
!
MODULE m
  IMPLICIT NONE
  INTERFACE OPERATOR(.ToString.)
    MODULE PROCEDURE tostring
  END INTERFACE OPERATOR(.ToString.)
CONTAINS
  FUNCTION tostring(arg)
    INTEGER, INTENT(IN) :: arg
    CHARACTER(:), ALLOCATABLE :: tostring
    allocate (character(5) :: tostring)
    write (tostring, "(I5)") arg
  END FUNCTION tostring
END MODULE m

  use m
  character(:), allocatable :: str
  integer :: i = 999
  str = .ToString. i
  if (str .ne. "  999") call abort
end

