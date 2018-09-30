! { dg-do run }
!
! Test the fix for PR70752 in which the type of the component 'c' is cast
! as character[1:0], which makes it slightly more difficult than usual to
! obtain the element length.  This is one and the same bug as PR72709.
!
! Contributed by Gilbert Scott  <gilbert.scott@easynet.co.uk>
!
PROGRAM TEST
  IMPLICIT NONE
  INTEGER, PARAMETER :: I = 3
  character (len = i), parameter :: str(5) = ['abc','cde','fgh','ijk','lmn']

  TYPE T
    CHARACTER(LEN=:), ALLOCATABLE :: C(:)
  END TYPE T
  TYPE(T), TARGET :: S
  CHARACTER (LEN=I), POINTER :: P(:)

  ALLOCATE ( CHARACTER(LEN=I) :: S%C(5) )
  s%c = str

! This PR uncovered several problems associated with determining the
! element length and indexing. Test fairly thoroughly!
  if (SIZE(S%C, 1) .ne. 5) stop 1
  if (LEN(S%C) .ne. 3) stop 2
  if (any (s%c .ne. str)) stop 3
  if (s%c(3) .ne. str(3)) stop 4
  P => S%C
  if (SIZE(p, 1) .ne. 5) stop 5
  if (LEN(p) .ne. 3) stop 6
  if (any (p .ne. str)) stop 7
  if (p(5) .ne. str(5)) stop 8
END PROGRAM TEST
