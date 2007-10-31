! { dg-do run }
! Tests the fix for PR33897, in which gfortran missed that the
! declaration of 'setbd' in 'nxtstg2' made it external.  Also
! the ENTRY 'setbd' would conflict with the external 'setbd'.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
MODULE ksbin1_aux_mod
 CONTAINS
  SUBROUTINE nxtstg1()
    INTEGER :: i
    i = setbd()  ! available by host association.
    if (setbd () .ne. 99 ) call abort ()
  END SUBROUTINE nxtstg1

  SUBROUTINE nxtstg2()
    INTEGER :: i
    integer :: setbd  ! makes it external.
    i = setbd()       ! this is the PR
    if (setbd () .ne. 42 ) call abort ()
  END SUBROUTINE nxtstg2

  FUNCTION binden()
    INTEGER :: binden
    INTEGER :: setbd
    binden = 0
  ENTRY setbd()
    setbd = 99
  END FUNCTION binden
END MODULE ksbin1_aux_mod

PROGRAM test
  USE ksbin1_aux_mod, only : nxtstg1, nxtstg2
  integer setbd ! setbd is external, since not use assoc.
  CALL nxtstg1()
  CALL nxtstg2()
  if (setbd () .ne. 42 ) call abort ()
  call foo
contains
  subroutine foo
    USE ksbin1_aux_mod ! module setbd is available
    if (setbd () .ne. 99 ) call abort ()
  end subroutine
END PROGRAM test

INTEGER FUNCTION setbd()
  setbd=42
END FUNCTION setbd

! { dg-final { cleanup-modules "ksbin1_aux_mod" } }
