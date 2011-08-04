! { dg-do compile }
!
! PR fortran/45745
! ICE with {L,U}BOUND intrinsic function as vector subscript on derived
! type component. 
!
! Original test by Joost Van de Vondele <Joost.VandeVondele@pci.uzh.ch>

MODULE pw_types
  TYPE pw_type
     REAL, DIMENSION ( : ), POINTER :: cr
  END TYPE pw_type
CONTAINS
  SUBROUTINE pw_write(pw)
    TYPE(pw_type), INTENT(in) :: pw
    PRINT *, pw%cr(LBOUND(pw%cr))
    PRINT *, pw%cr(UBOUND(pw%cr))
  END SUBROUTINE pw_write
END MODULE

! { dg-final { cleanup-modules "pw_types" } }
