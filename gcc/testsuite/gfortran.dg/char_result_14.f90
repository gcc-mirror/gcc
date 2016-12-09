! { dg-do run }
!
! Tests the fix for PR44265. This is the original test with the addition
! of the check of the issue found in comment #1 of the PR.
!
! Contributed by Ian Harvey  <ian_harvey@bigpond.com>
! Ian also contributed the first version of the fix.
!
! The original version of the bug
MODULE Fruits0
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Get0
CONTAINS
  FUNCTION Get0(i) RESULT(s)
    CHARACTER(*), PARAMETER :: names(3) = [  &
        'Apple  ',  &
        'Orange ',  &
        'Mango  ' ];
    INTEGER, INTENT(IN) :: i
    CHARACTER(LEN_TRIM(names(i))) :: s
    !****
    s = names(i)
  END FUNCTION Get0
END MODULE Fruits0
!
! Version that came about from sorting other issues.
MODULE Fruits
  IMPLICIT NONE
  PRIVATE
    character (20) :: buffer
    CHARACTER(*), PARAMETER :: names(4) = [  &
        'Apple  ',  &
        'Orange ',  &
        'Mango  ',  &
        'Pear   ' ];
  PUBLIC :: Get, SGet, fruity2, fruity3, buffer
CONTAINS
! This worked previously
  subroutine fruity3
    write (buffer, '(i2,a)') len (Get (4)), Get (4)
  end
! Original function in the PR
  FUNCTION Get(i) RESULT(s)
    INTEGER, INTENT(IN) :: i
    CHARACTER(LEN_trim(names(i))) :: s
    !****
    s = names(i)
  END FUNCTION Get
! Check that dummy is OK
  Subroutine Sget(i, s)
    CHARACTER(*), PARAMETER :: names(4) = [  &
        'Apple  ',  &
        'Orange ',  &
        'Mango  ',  &
        'Pear   ' ];
    INTEGER, INTENT(IN) :: i
    CHARACTER(LEN_trim(names(i))), intent(out) :: s
    !****
    s = names(i)
    write (buffer, '(i2,a)') len (s), s
  END subroutine SGet
! This would fail with undefined references to mangled 'names' during linking
  subroutine fruity2
    write (buffer, '(i2,a)') len (Get (3)), Get (3)
  end
END MODULE Fruits

PROGRAM WheresThatbLinkingConstantGone
  use Fruits0
  USE Fruits
  IMPLICIT NONE
  character(7) :: arg = ""
  integer :: i

! Test the fix for the original bug
  if (len (Get0(1)) .ne. 5) call abort
  if (Get0(2) .ne. "Orange") call abort

! Test the fix for the subsequent issues
  call fruity
  if (trim (buffer) .ne. " 6Orange") call abort
  call fruity2
  if (trim (buffer) .ne. " 5Mango") call abort
  call fruity3
  if (trim (buffer) .ne. " 4Pear") call abort
  do i = 3, 4
    call Sget (i, arg)
    if (i == 3) then
      if (trim (buffer) .ne. " 5Mango") call abort
      if (trim (arg) .ne. "Mango") call abort
    else
      if (trim (buffer) .ne. " 4Pear") call abort
! Since arg is fixed length in this scope, it gets over-written
! by s, which in this case is length 4. Thus, the 'o' remains.
      if (trim (arg) .ne. "Pearo") call abort
    end if
  enddo
contains
  subroutine fruity
      write (buffer, '(i2,a)') len (Get (2)), Get (2)
  end
END PROGRAM WheresThatbLinkingConstantGone
