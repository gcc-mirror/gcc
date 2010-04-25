! { dg-options "-O2 -fgraphite-identity " }

MODULE qs_ks_methods
  INTEGER, PARAMETER :: sic_list_all=1
  TYPE dft_control_type
     INTEGER :: sic_list_id
  END TYPE
CONTAINS
  SUBROUTINE sic_explicit_orbitals( )
    TYPE(dft_control_type), POINTER          :: dft_control
    INTEGER, ALLOCATABLE, DIMENSION(:, :)    :: sic_orbital_list
    INTEGER, DIMENSION(:), &
      POINTER                                :: mo_derivs
    SELECT CASE(dft_control%sic_list_id)
    CASE(sic_list_all)
      DO i=1,k_alpha
         IF (SIZE(mo_derivs,1)==1) THEN
         ELSE
             sic_orbital_list(3,iorb)=2
         ENDIF
      ENDDO
    END SELECT
    CALL test()
  END SUBROUTINE sic_explicit_orbitals
END MODULE qs_ks_methods
! { dg-final { cleanup-modules "qs_ks_methods" } }
