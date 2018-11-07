! { dg-do run }
!
! Tests the fix for PR85603.
!
! Contributed by Walt Spector  <w6ws@earthlink.net>
!_____________________________________________
! Module for a test against a regression that occurred with
! the first patch for this PR.
!
MODULE TN4
  IMPLICIT NONE
  PRIVATE
  INTEGER,PARAMETER::SH4=KIND('a')
  TYPE,PUBLIC::TOP
    CHARACTER(:,KIND=SH4),ALLOCATABLE::ROR
    CHARACTER(:,KIND=SH4),ALLOCATABLE::VI8
  CONTAINS
    PROCEDURE,NON_OVERRIDABLE::SB=>TPX
  END TYPE TOP
CONTAINS
  SUBROUTINE TPX(TP6,PP4)
    CLASS(TOP),INTENT(INOUT)::TP6
    INTEGER,INTENT(IN)::PP4
    TP6%ROR=TP6%ROR(:PP4-1)
    TP6%VI8=TP6%ROR(:PP4-1)
  END SUBROUTINE TPX
END MODULE TN4
!_____________________________________________
!
program strlen_bug
  implicit none

  character(:), allocatable :: strings(:)
  integer :: maxlen

  strings = [ character(32) ::  &
      'short',  &
      'somewhat longer' ]
  maxlen = maxval (len_trim (strings))
  if (maxlen .ne. 15) stop 1

! Used to cause an ICE and in the later version of the problem did not reallocate.
  strings = strings(:)(:maxlen)
  if (any (strings .ne. ['short          ','somewhat longer' ])) stop 2
  if (len (strings) .ne. maxlen) stop 3

! Try something a bit more complicated.
  strings = strings(:)(2:maxlen - 5)
  if (any (strings .ne. ['hort     ','omewhat l' ])) stop 4
  if (len (strings) .ne. maxlen - 6) stop 5

  deallocate (strings)          ! To check for memory leaks

! Test the regression, noted by Dominique d'Humieres is fixed.
! Referenced in https://groups.google.com/forum/#!topic/comp.lang.fortran/nV3TlRlVKBc
!
  call foo
contains
  subroutine foo
    USE TN4
    TYPE(TOP) :: Z

    Z%ROR = 'abcd'
    call Z%SB (3)
    if (Z%VI8 .ne. 'ab') stop 6
end

end program
