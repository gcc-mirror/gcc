! { dg-do run }
! Tests the fix for PR26393, in which an ICE would occur in trans-decl.c
! (gfc_get_symbol_decl) because anzKomponenten is not referenced in the
! interface for solveCConvert. The solution was to assert that the symbol
! is either referenced or in an interface body.
!
! Based on the testcase in the PR.
!
  MODULE MODULE_CONC
    INTEGER, SAVE :: anzKomponenten = 2
  END MODULE MODULE_CONC

  MODULE MODULE_THERMOCALC
    INTERFACE
      FUNCTION solveCConvert ()
        USE MODULE_CONC, ONLY: anzKomponenten
        REAL :: solveCConvert(1:anzKomponenten)
        END FUNCTION solveCConvert
    END INTERFACE
  END MODULE MODULE_THERMOCALC

  SUBROUTINE outDiffKoeff
    USE MODULE_CONC
    USE MODULE_THERMOCALC
    REAL :: buffer_conc(1:anzKomponenten)
    buffer_conc = solveCConvert ()
    if (any(buffer_conc .ne. (/(real(i), i = 1, anzKomponenten)/))) &
          call abort ()
  END SUBROUTINE outDiffKoeff

  program missing_ref
    USE MODULE_CONC
    call outDiffKoeff
! Now set anzKomponenten to a value that would cause a segfault if
! buffer_conc and solveCConvert did not have the correct allocation
! of memory.
    anzKomponenten = 5000
    call outDiffKoeff
  end program missing_ref
 
  FUNCTION solveCConvert ()
    USE MODULE_CONC, ONLY: anzKomponenten
    REAL :: solveCConvert(1:anzKomponenten)
    solveCConvert = (/(real(i), i = 1, anzKomponenten)/)
  END FUNCTION solveCConvert

! { dg-final { cleanup-modules "module_conc module_thermocalc" } }
