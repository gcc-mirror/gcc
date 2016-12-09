! { dg-do run }
!
! Tests the fix for PR44265. This test arose because of an issue found
! during the development of the fix; namely the clash between the normal
! module parameter and that found in the specification expression for
! 'Get'.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
MODULE Fruits
  IMPLICIT NONE
  PRIVATE
  character (20) :: buffer
  PUBLIC :: Get, names, fruity, buffer
    CHARACTER(len=7), PARAMETER :: names(3) = [  &
        'Pomme  ',  &
        'Orange ',  &
        'Mangue ' ];
CONTAINS
  FUNCTION Get(i) RESULT(s)
    CHARACTER(len=7), PARAMETER :: names(3) = [  &
        'Apple  ',  &
        'Orange ',  &
        'Mango  ' ];
    INTEGER, INTENT(IN) :: i
    CHARACTER(LEN_TRIM(names(i))) :: s
    s = names(i)
  END FUNCTION Get
  subroutine fruity (i)
    integer :: i
  write (buffer, '(i2,a)') len (Get (i)), Get (i)
  end subroutine
END MODULE Fruits

PROGRAM WheresThatbLinkingConstantGone
  USE Fruits
  IMPLICIT NONE
  integer :: i
  write (buffer, '(i2,a)') len (Get (1)), Get (1)
  if (trim (buffer) .ne. " 5Apple") call abort
  call fruity(3)
  if (trim (buffer) .ne. " 5Mango") call abort
  if (trim (names(3)) .ne. "Mangue") Call abort
END PROGRAM WheresThatbLinkingConstantGone
