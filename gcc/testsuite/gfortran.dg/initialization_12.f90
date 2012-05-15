! { dg-do compile }
! PR fortran/32945 - ICE in init expressions
!
! Contributed by Florian Ladstaedter <flad AT gmx DOT at>
!

MODULE EGOPS_Utilities
CONTAINS
  FUNCTION dirname(fullfilename)
    Character(LEN=*),  Intent(In)    :: fullfilename
    Character(LEN=LEN(fullfilename)) :: dirname
    dirname = ''
  END FUNCTION
END MODULE EGOPS_Utilities

MODULE AtmoIono
  CHARACTER(LEN=10), PARAMETER :: ComputeDryAtmModel = 'Dry Atm.  '

  type AtmModel
    character (len=len(ComputeDryAtmModel)) :: moistDryStr
  end type AtmModel
END MODULE AtmoIono

module AtmoIonoSphere
  use EGOPS_Utilities
  use AtmoIono
end module AtmoIonoSphere
